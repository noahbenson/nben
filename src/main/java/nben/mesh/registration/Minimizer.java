////////////////////////////////////////////////////////////////////////////////////////////////////
// Minimizer.java
//
// The nben.mesh.registration namespace contains functions related to the registration of 
// surface meshes to models defined on the cortical surface; it is designed to work with front-end 
// neuroscience libraries in other languages, such as the Mathematica Neurotica`Registration 
// namespace of the Neurotica library (see https://github.com/noahbenson/Neurotica/).
//
// Copyright (C) 2016 by Noah C. Benson.
// This file is part of the nben JVM library.
//
// This program is free software: you can redistribute it and/or modify it under the terms of the
// GNU General Public License as published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
// the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
// If not, see <http://www.gnu.org/licenses/>.

package nben.mesh.registration;

import nben.mesh.registration.IPotentialField;
import nben.mesh.registration.PotentialValue;

import nben.util.Par;
import nben.util.Numbers;

import java.util.Arrays;

/** Minimizer is the class that handles a specific registration minimization. Minimizer
 *  has two basic modes of operation: step and nimbleStep. The step function is more
 *  straight-forward than the nimbleStep function, but they are generally similar. Both attempt
 *  to move a certain distance along the negative-gradient toward the minimum; step does this for
 *  all vertices at once while nimbleStep does this for the vertices in small groups where each
 *  group is chosen based on the length of the gradient at a particular point. Generally speaking,
 *  the step function is a more traditional gradient-descent search, while the nimbleStep is a bit
 *  faster.
 *  During minimization, the Minimizer object keeps track of a set of coordinates for each vertex,
 *  starting with an initial position, X0. As the minimization proceeds, this coordinate matrix is
 *  updated to be closer to the minimum.
 *
 *  @author Noah C. Benson
 */
public class Minimizer {

   ////////////////////////////////////////////////////////////////////////////////
   // Private Classes

   // StepWorker takes a step along the gradient
   private class StepWorker implements Runnable {
      public final int id;
      public final int workers;
      public int[] subset;      // intention is that this gets set by Minimizer during steps
      public double stepSize;   // the step size to take; negative to minimize, positive to revert
      public double[][] gradient;
      public double[][] X;
      
      public StepWorker(int i, int nwork) {
         id = i;
         workers = nwork;
         subset = null;
         stepSize = -1.0;
      }
      public void run() {
         int i, j, u;
         if (subset == null) {
            u = m_X[0].length;
            for (j = 0; j < X.length; ++j) {
               for (i = id; i < u; i += workers)
                  X[j][i] += gradient[j][i] * stepSize;
            }
         } else {
            for (i = id; i < subset.length; i += workers) {
               u = subset[i];
               for (j = 0; j < X.length; ++j)
                  X[j][u] += gradient[j][u]*stepSize;
            }
         }
      }
   }
   // RevertWorker replaces data in a coordinate matrix
   private class RevertWorker implements Runnable {
      public final int id;
      public final int workers;
      public int[] subset;      // intention is that this gets set by Minimizer during steps
      public double[][] X;      // dest matrix
      public double[][] X0;     // source matrix
      public RevertWorker(int i, int nwork) {
         id = i;
         workers = nwork;
         subset = null;
      }
      public void run() {
         if (subset == null) {
            int n = m_X[0].length, j;
            int mn = (int)(n *  id     / (double)workers);
            int ct = (int)(n * (id + 1)/ (double)workers) - mn;
            for (j = 0; j < m_X.length; ++j)
               System.arraycopy(X0[j], mn, X[j], mn, ct);
         } else {
            int u, i, j;
            for (i = id; i < subset.length; i += workers) {
               u = subset[i];
               for (j = 0; j < m_X.length; ++j)
                  X[j][u] = X0[j][u];
            }
         }
      }
   }
   // The ZeroWorker class is used by the class to zero the gradient quickly
   private class ZeroWorker implements Runnable {
      int id;
      int workers;
      double[][] gradient;
      int[] subset;
      public ZeroWorker(int i, int ws) {id = i; workers = ws;}
      public void run() {
         int i, j;
         double[] grad;
         for (j = 0; j < gradient.length; ++j) {
            grad = gradient[j];
            if (subset == null) {
               for (i = id; i < grad.length; i += workers)
                  grad[i] = 0;
            } else {
               for (i = id; i < subset.length; i += workers)
                  grad[subset[i]] = 0;
            }
         }
      }
   }


   /** The Minimizer.Report class stores the data from a minimization trajectory and is returned
    *  by the step function. In the case of the step function, a report datum is filed every step,
    *  while in the case of the nimbleStep function, it is filed every major-step.
    */
   public static class Report {
      public double[] stepSizes;
      public double[] stepLengths;
      public double[] steepestVertexGradientNorms;
      public double[] potentialChanges;
      public double initialPotential;
      public double finalPotential;
      public int steps;

      private Report(double pe0) {
         steps = 0;
         stepSizes = new double[16];
         stepLengths = new double[16];
         steepestVertexGradientNorms = new double[16];
         potentialChanges = new double[16];
         initialPotential = pe0;
         finalPotential = 0;
         steps = 0;
      }
      private double[] extend(double[] a) {
         if (a == null || a.length == 0)
            return new double[16];
         double[] tmp = new double[2*a.length];
         System.arraycopy(a, 0, tmp, 0, a.length);
         return tmp;
      }
      private double[] trim(double[] a, int mx) {
         double[] b = new double[mx];
         System.arraycopy(a, 0, b, 0, mx);
         return b;
      }
      private void freeze(double pe) {
         finalPotential = pe;
         stepSizes = trim(stepSizes, steps);
         stepLengths = trim(stepLengths, steps);
         steepestVertexGradientNorms = trim(steepestVertexGradientNorms, steps);
         potentialChanges = trim(potentialChanges, steps);
      }
      private void push(double dt, double dx, double maxnorm, double dpe) {
         if (steps == stepSizes.length) {
            stepSizes = extend(stepSizes);
            stepLengths = extend(stepLengths);
            steepestVertexGradientNorms = extend(steepestVertexGradientNorms);
            potentialChanges = extend(potentialChanges);
         }
         stepSizes[steps] = dt;
         stepLengths[steps] = dx;
         steepestVertexGradientNorms[steps] = maxnorm;
         potentialChanges[steps] = dpe;
         steps++;
      }
   }

   ////////////////////////////////////////////////////////////////////////////////
   // Private Data
   private IPotentialField    m_field;
   private double[][]         m_X0;
   private double[][]         m_X;
   // workspace used by this object
   private StepWorker[]       m_stepWorkers;
   private RevertWorker[]     m_revertWorkers;
   private ZeroWorker[]       m_zeroWorkers;
   // public data meant to be examined by outer callers in the case of an error
   public Report report;
   

   ////////////////////////////////////////////////////////////////////////////////
   // Accessors
   synchronized public IPotentialField getPotentialField() {return m_field;}
   synchronized public double[][] getX0() {return m_X0;}
   synchronized public double[][] getX() {return m_X;}

   /** Constructs a Minimizer that attempts to minimize vertex coordinates starting at
    *  the position given by X0 in the context of the potential field pfn.
    *
    *  @param pfn The potential field within which to minimize the vertex coordinates
    *  @param X0 The coordinate matrix at which to start the minimization
    */
   public Minimizer(IPotentialField pfn, double[][] X0) {
      m_field = pfn;
      int dims = X0.length;
      int n = X0[0].length;
      m_X0 = new double[dims][n];
      m_X = new double[dims][n];
      for (int k = 0; k < dims; ++k) {
         System.arraycopy(X0[k], 0, m_X0[k], 0, n);
         System.arraycopy(X0[k], 0, m_X[k], 0, n);
      }
      m_zeroWorkers = new ZeroWorker[Par.workers()];
      m_stepWorkers = new StepWorker[Par.workers()];
      m_revertWorkers = new RevertWorker[Par.workers()];
      for (int i = 0; i < m_stepWorkers.length; ++i) {
         m_zeroWorkers[i] = new ZeroWorker(i, Par.workers());
         m_stepWorkers[i] = new StepWorker(i, Par.workers());
         m_revertWorkers[i] = new RevertWorker(i, Par.workers());
      }
      report = null;
   }

   /** min.currentPotential() yields a PotentialValue object for the current position of the vertex
    *  coordinates in the Minimizer min.
    *
    *  @param grad a matrix in which to store the gradient
    *  @param norms a vector in which to store the gradient norms by vertex
    *  @param ss the subset of vertices over which this calculation should occur
    *  @return a PotentialValue object that encapsulates the current minimizer point
    */
   synchronized public PotentialValue currentPotential(double[][] grad, double[] norms) 
      throws Exception {
      return new PotentialValue(m_field, null, m_X, grad, norms);
   }
   synchronized public PotentialValue currentPotential(double[][] grad) throws Exception {
      return new PotentialValue(m_field, null, m_X, grad, null);
   }
   synchronized public PotentialValue currentPotential(double[] norms) throws Exception {
      return new PotentialValue(m_field, null, m_X, null, norms);
   }
   synchronized public PotentialValue currentPotential() throws Exception {
      return new PotentialValue(m_field, null, m_X, null, null);
   }
   synchronized public PotentialValue currentPotential(double[][] grad, double[] norms, int[] ss) 
      throws Exception {
      return new PotentialValue(m_field, ss, m_X, grad, norms);
   }
   synchronized public PotentialValue currentPotential(double[][] grad, int[] ss) throws Exception {
      return new PotentialValue(m_field, ss, m_X, grad, null);
   }
   synchronized public PotentialValue currentPotential(double[] norms, int[] ss) throws Exception {
      return new PotentialValue(m_field, ss, m_X, null, norms);
   }
   synchronized public PotentialValue currentPotential(int[] ss) throws Exception {
      return new PotentialValue(m_field, ss, m_X, null, null);
   }

   /** min.potentialAt(X) yields a PotentialValue object for the vertex positions given in the 
    *  coordinate matrix X, which must be size (d x n) where d is the dimensionality of the
    *  embedding space of the minimizer and n is the number of vertices.
    *
    *  @param X a coordinate matrix at which to find the potential
    *  @return a PotentialValue object that encapsulates the potential of the point given by X
    */
   public PotentialValue potentialAt(double[][] X) throws Exception {
      return new PotentialValue(m_field, null, X, null, null);
   }

   ////////////////////////////////////////////////////////////////////////////////
   // Private Helper Functions
   synchronized private void takeStep(double[][] X, double dt, double[][] grad, int[] subset)
      throws Exception {
      for (int i = 0; i < m_stepWorkers.length; ++i) {
         m_stepWorkers[i].subset = subset;
         m_stepWorkers[i].X = X;
         m_stepWorkers[i].gradient = grad;
         m_stepWorkers[i].stepSize = -dt;
      }
      Par.run(m_stepWorkers);
   }
   synchronized private void copyMatrix(double[][] X, double[][] X0, int[] subset)
      throws Exception {
      for (int i = 0; i < m_stepWorkers.length; ++i) {
         m_revertWorkers[i].subset = subset;
         m_revertWorkers[i].X = X;
         m_revertWorkers[i].X0 = X0;
      }
      Par.run(m_revertWorkers);
   }
   synchronized private void copyMatrix(double[][] X, double[][] X0) throws Exception {
      copyMatrix(X, X0, null);
   }

   synchronized private void clearGradient(double[][] grad, int[] ss) throws Exception {
      for (int i = 0; i < m_zeroWorkers.length; ++i) {
         m_zeroWorkers[i].gradient = grad;
         m_zeroWorkers[i].subset = ss;
      }
      Par.run(m_zeroWorkers);
   }
   synchronized private void clearGradient(double[][] grad) throws Exception {
      clearGradient(grad, null);
   }

   ////////////////////////////////////////////////////////////////////////////////
   // The Step Functions
   /** min.step(dt, ms, z) follows the gradient of its potential-field and configuration until it 
    *  has either traveled for dt units of time or has taken ms steps in such a way that no vertex
    *  ever moves more than distance z in a single step. 
    *  On error, an exception is thrown, most likely due to a problem with multi-threading.
    *
    *  @param deltaT the maximum 'time' to travel (the maximum number of gradient-lengths to step)
    *  @param maxSteps the maximum number of steps to take during the minimization
    *  @param z the maximum distance any single vertex should ever travel during a step
    *  @return a Report object detailing the minimization trajectory
    */
   synchronized public Report step(double deltaT, int maxSteps, double z) throws Exception {
      double t, t0, dt, dx, pe0, maxNorm;
      int k = 0;
      if (deltaT <= 0 || maxSteps < 1) return null;
      if (z <= 0) throw new IllegalArgumentException("parameter z to step must be > 0");
      // buffers in which we store gradient and gradient norms...
      double[][] grad     = new double[m_X.length][m_X[0].length];
      double[][] gradTmp  = new double[m_X.length][m_X[0].length];
      double[]   norms    = new double[m_X[0].length];
      double[]   normsTmp = new double[m_X[0].length];
      double[][] Xbak     = new double[m_X.length][m_X[0].length];
      double[][] buf2;
      double[]   buf1;
      // first thing: calculate the total gradient!
      PotentialValue val = currentPotential(grad, norms);
      PotentialValue valTmp;
      maxNorm = norms[val.steepestVertex];
      if (Double.isNaN(val.potential))
         throw new IllegalArgumentException("Initial state has a NaN potential");
      else if (Double.isInfinite(val.potential))
         throw new IllegalArgumentException("Initial state has a non-finite potential");
      // also, save a backup X
      copyMatrix(Xbak, m_X, null);
      // okay, iteratively take appropriately-sized steps...
      t = 0;
      pe0 = val.potential;
      Report re = new Report(pe0);
      try {
         while (t < deltaT && k < maxSteps) {
            if (Numbers.zeroish(maxNorm))
               throw new Exception("gradient is effectively 0");
            // pick our start step size; first would be z/maxNorm or timeLeft, whichever is smaller
            dt = z / maxNorm;
            if (dt + t > deltaT) dt = deltaT - t;
            t0 = t;
            // see if the current step-size works; if not we'll halve it and try again...
            while (t0 == t) {
               // make sure we aren't below a threshold...
               if (Numbers.zeroish(dt)) throw new Exception("Step-size decreased to effectively 0");
               // take a step; this copies the current coordinates (m_X) into m_X0; same for grad
               takeStep(m_X, dt, grad, null);
               // now, get the new potential value...
               clearGradient(gradTmp);
               valTmp = currentPotential(gradTmp, normsTmp);
               // see if this was a valid step...
               if (Double.isNaN(valTmp.potential)) {
                  throw new IllegalStateException("Potential function yielded NaN");
               } else if (Double.isInfinite(valTmp.potential) || valTmp.potential >= pe0) {
                  // we broke a triangle or we failed to reduce potential (perhaps due to a 
                  // too-large step-size); swap x0 back to x and grad0 back to grad and try with a
                  // smaller step
                  copyMatrix(m_X, Xbak, null);
                  dt *= 0.5;
               } else {
                  // We've completed a step!
                  ++k;
                  // replace the various progress-tracking value with the new ones
                  val = valTmp;
                  buf2 = grad;
                  grad = gradTmp;
                  gradTmp = buf2;
                  buf1 = norms;
                  norms = normsTmp;
                  normsTmp = buf1;
                  maxNorm = norms[val.steepestVertex];
                  copyMatrix(Xbak, m_X, null);
                  // push this step onto the report...
                  re.push(dt, val.gradientLength * dt, maxNorm, val.potential - pe0);
                  // update the time, total distance, and potential
                  t += dt;
                  pe0 = val.potential;
               }
            }
         }
      } finally {
         // freeze the report and save it...
         re.freeze(val == null? pe0 : val.potential);
         report = re;
      }
      return re;
   }

   /** min.nimpleStep(dt, ms, z, p) follows the gradient of its potential-field and configuration
    *  until it has either traveled for dt units of time or has taken ms steps in such a way that no
    *  vertex ever moves more than distance z in a single step. 
    *  On error, an exception is thrown, most likely due to a problem with multi-threading.
    *  The nimbleStep method is basically identical to the step method except that it takes an
    *  additional parameter p, which tells it how many partitions to divide the vertices into. Each
    *  step, the nimbleStep algorithm first calculates the overall gradient then divides the
    *  gradient into p groups such that the first group has all the vertices with high gradient 
    *  norms and the last group has all the vertices with small gradient norms. Each group is then
    *  updated separately from the others in such a fashion that the vertices with large gradients
    *  are updated more frequently than vertices with low gradients. Practically, this allows each
    *  step to get more done with less work.
    *
    *  @param deltaT the maximum 'time' to travel (the maximum number of gradient-lengths to step)
    *  @param maxSteps the maximum number of steps to take during the minimization
    *  @param z the maximum distance any single vertex should ever travel during a step
    *  @param partitions the number of partitions to divide the vertices into (should be small; i.e.
    *                    no more than ~13).
    *  @return a Report object detailing the minimization trajectory
    */
   synchronized public Report nimbleStep(double deltaT, int maxSteps, double z, int partitions)
      throws Exception {
      double maxNorm, t, t0, dt, dt0, dx, pe0, peStep, dtStep;
      int miniStepsPerStep = (1 << partitions);
      int k = 0;
      int miniStep, part;
      boolean cont;
      // fields that get partitioned up
      IPotentialField[] fields = new IPotentialField[partitions];
      int[][] ss;
      if (deltaT <= 0) return null;
      if (z <= 0) throw new IllegalArgumentException("parameter z to step must be > 0");
      // buffers in which we store gradient and gradient norms...
      double[][] grad     = new double[m_X.length][m_X[0].length];
      double[][] gradTmp  = new double[m_X.length][m_X[0].length];
      double[]   norms    = new double[m_X[0].length];
      double[]   normsTmp = new double[m_X[0].length];
      double[][] Xbak     = new double[m_X.length][m_X[0].length];
      double[]   dtPart   = new double[partitions];
      // first thing: calculate the total gradient!
      PotentialValue val, valTmp;
      val = currentPotential(grad, norms);
      if (Double.isNaN(val.potential))
         throw new IllegalArgumentException("Initial state has a NaN potential");
      else if (Double.isInfinite(val.potential))
         throw new IllegalArgumentException("Initial state has a non-finite potential");
      // now make a copy of X
      copyMatrix(Xbak, m_X, null);
      // okay, iteratively take appropriately-sized overall-steps...
      maxNorm = norms[val.steepestVertex];
      pe0 = val.potential;
      dx = 0;
      t = 0;
      Report re = new Report(pe0);
      try {
         while (t < deltaT && k++ < maxSteps) {
            if (Numbers.zeroish(maxNorm))
               throw new Exception("gradient is effectively 0");
            Arrays.fill(dtPart, 0);
            // pick our start step size; first would be z/maxNorm or timeLeft, whichever is smaller
            dt0 = z / maxNorm;
            if (dt0 + t > deltaT) dt0 = deltaT - t;
            t0 = t;
            // we want to make some substeps to run through...
            ss = substeps(norms, partitions);
            for (part = 0; part < partitions; part++)
               fields[part] = m_field.subfield(ss[part]);
            // okay, we make <ministepsPerStep> steps total...
            for (miniStep = 0; miniStep < miniStepsPerStep; ++miniStep) {
               // on this mini-step, we update the appropriate subsets using a max step-size of dt
               // scaled up to be appropriate for how often this subset is updated;
               for (part = 0; part < partitions; ++part) {
                  // only do this part if it is divisible by the appropriate power
                  if ((miniStep + 1) % (1 << part) > 0) continue;
                  // we need to recalculate potential etc, as it may have changed...
                  clearGradient(grad, ss[part]);
                  valTmp = new PotentialValue(fields[part], ss[part], m_X, grad, norms);
                  // also skip this part if the gradient is basically 0
                  if (Numbers.zeroish(valTmp.gradientLength)) continue;
                  // we always start with this stepsize, scaled up based on partition number...
                  dt = dt0 * (1 << part);
                  // see if the current step-size works; if not we'll halve it and try again...
                  peStep = valTmp.potential;
                  cont = true;
                  while (cont) {
                     // make sure we aren't below a threshold...
                     if (Numbers.zeroish(dt))
                        throw new Exception("Step-size decreased to effectively 0");
                     // take a single step...
                     takeStep(m_X, dt, grad, ss[part]);
                     // calculate the new gradient/potential
                     valTmp = new PotentialValue(fields[part], ss[part], m_X, gradTmp, normsTmp);
                     if (Double.isNaN(valTmp.potential)) {
                        throw new IllegalStateException("Potential function yielded NaN");
                     } else if (Double.isInfinite(valTmp.potential) || peStep < valTmp.potential) {
                        // we broke a triangle or we failed to reduce potential (perhaps due to a 
                        // too-large step-size); swap x0 back to x and grad0 back to grad and try 
                        // with a smaller step
                        copyMatrix(m_X, Xbak, ss[part]);
                        dt *= 0.5;
                     } else {
                        // the step was a success!
                        copyMatrix(Xbak, m_X, ss[part]);
                        dtPart[part] += dt;
                        cont = false;
                     }
                  }
               }
            }
            // we've completed a series of miniSteps -- that means we've made a Step!
            valTmp = val;
            Arrays.sort(dtPart);
            dtStep = dtPart[partitions - 1];
            t += dtStep;
            val = currentPotential(grad, norms);
            pe0 = val.potential;
            re.push(dtStep, val.gradientLength * dtStep, maxNorm, valTmp.potential - pe0);
            maxNorm = norms[val.steepestVertex];
         }
      } finally {
         re.freeze(val == null? pe0 : val.potential);
         report = re;
      }
      return re;
   }

   // nimbleStep uses this function to partition the gradNorm's into a plan of action:
   /** min.substeps(norms, p) examines the given gradient norms and partitions them by vertex into a
    *  sequence of p fields each of which operates over an independent subsets of the vertices; for
    *  the next 2^p steps, the recommended subsets to use on step k are the subsets represented in
    *  R[i] for every i &lt; p such that mod(k, 2^i) == 0, where R is the return value of this
    *  function.
    *
    *  @param norms an array of the gradient norms by which to sort/partition vertices
    *  @param k the number of partitions to create
    *  @return an array of unique subsets of the vertices based on their gradient lengths
    */
   protected int[][] substeps(double[] norms, int k) {
      if (k < 2 || k > 32) throw new IllegalArgumentException("steps must be in the range 2-32");
      int i, j, n = norms.length;
      int steps = (1 << k);
      IPotentialField[] fields = new IPotentialField[k];
      // first, sort the gradient norms... this gives us the cutoffs
      double[] gnorms = new double[n];
      System.arraycopy(norms, 0, gnorms, 0, gnorms.length);
      Arrays.sort(gnorms);
      // now we can go ahead and get the bucket sizes...
      int[][] buckets = new int[k][];
      int[] ranks = new int[k];
      double[] cutoffs = new double[k];
      int left = n;
      for (i = 0; i < k-1; ++i) {
         buckets[i] = new int[n / (1 << (k - i))];
         left -= buckets[i].length;
         ranks[i] = left;
         cutoffs[i] = gnorms[left]; // must be >= than cutoff[i] to be in i
      }
      buckets[k-1] = new int[left];
      ranks[k-1] = 0;
      cutoffs[k-1] = gnorms[0];
      // now we put each index in its place
      int[] counts = new int[k];
      double tmp;
      for (i = 0; i < n; ++i) {
         tmp = norms[i];
         for (j = 0; j < k; ++j) {
            if (j == k-1 || (tmp >= cutoffs[j] && counts[j] < buckets[j].length)) {
               buckets[j][counts[j]++] = i;
               break;
            }
         }
      }
      // okay, just return the buckets...
      return buckets;
   }
   public int[][] substeps(double[] norms) {return substeps(norms, 8);}

}

