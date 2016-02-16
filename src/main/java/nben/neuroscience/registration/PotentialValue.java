////////////////////////////////////////////////////////////////////////////////////////////////////
// PotentialValue.java
//
// The nben.neuroscience.registration namespace contains functions related to the registration of 
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

package nben.neuroscience.registration;

import nben.neuroscience.registration.Util;
import nben.neuroscience.registration.IPotentialField;
import nben.util.Par;
import nben.util.Numbers;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

/** The PotentialValue class stores data related to the potential and gradient value at a particular
 *  point in a potential field. When constructed, the potential value object automatically
 *  fills in various data such as the maximum gradient norm across vertices and the overall gradient
 *  length.
 *
 *  @author Noah C. Benson
 */
public final class PotentialValue {
   /** The field is the PotentialField over which the potential and gradient stored by the given
    *  potential value object was calculated.
    */
   public final IPotentialField field;
   /** subset is an array of vertex id's that were considered in the generation of this potential
    *  value; note that this is not modified or allocated by the PotentialValue object.
    */
   public final int[] subset;
   /** X is a (dims x vertices)-sized matrix from which the calculator obtains the coordinates for
    *  the potential and gradient calculations; note that this is not modified or allocated by the
    *  PotentialValue object.
    */
   public final double[][] X;
   /** potential is the value of the potential that is set by the calculate() function, according
    *  to the vertex configuration of the relevant subset of vertices in matrix X (given by members 
    *  subset and X) in the given potential field (stored in the member field).
    */
   public final double potential;
   /** gradient is a (dims x vertices)-sized matrix in which the gradient values for each vertex are
    *  stored for the given potential field at the given coordinates for the given subset of 
    *  vertices (see members field, X, and subset). Although gradient is sized for the entire vertex
    *  list (and not just the subset), the values at vertices not in the subset is undefined.
    */
   public final double[][] gradient;
   /** gradientNorms is a (vertices)-sized vector of the norm of the gradient at each vertex; it is
    *  automatically calculated on construction of the PotentialValue object.
    */
   public final double[] gradientNorms;
   /** steepestVertex is an index into gradientNorms that indicates the vertex with the steepest
    *  gradient
    */
   public final int steepestVertex;
   /** gradientLength is the total length of the (flattened) gradient vector.
    */
   public final double gradientLength;

   // The GradWorker class is used by the constructor to calculate the gradient norms and length
   // in parallel when the subset of vertices to examine is large enough to merit parallelization
   private class GradWorker implements Runnable {
      int id;
      int workers;
      int maxgrad;
      double gradlen2;

      public GradWorker(int i, int ws) {id = i; workers = ws;}

      public void run() {
         double tmp, maxgradlen;
         int i, j, u;
         gradlen2 = 0;
         for (i = id; i < subset.length; i += workers)
            gradientNorms[subset[i]] = 0;
         for (j = 0; j < gradient.length; ++j) {
            for (i = id; i < subset.length; i += workers) {
               u = subset[i];
               tmp = gradient[j][u];
               tmp *= tmp;
               gradientNorms[u] += tmp;
               gradlen2 += tmp;
            }
         }
         for (i = id; i < subset.length; i += workers) {
            u = subset[i];
            gradientNorms[u] = Math.sqrt(gradientNorms[u]);
            if (i == id || gradientNorms[u] > maxgradlen) {
               maxgradlen = gradientNorms[u];
               maxgrad = u;
            }
         }
      }
   }

   /** Constructs a PotentialValue object. The first (field) and third (X) parameters are required,
    *  but all other parameters may be omitted, in which case they are treated as null.
    *
    *  @param field the potential field over which to calculate the potential and gradient
    *  @param subset the subset of vertices at which to calculate the potential; if null, then the
    *                entire set of vertices is used
    *  @param X the coordinates at which to calculate the potential
    *  @param gradient the array into which to place the gradient values; if null, then an 
    *                  array is automatically allocated
    *  @param gradientNorms the array into which to place the gradient norms; if null, then an
    *                       array is automatically allocated
    *  @throws IllegalArgumentException if the given arguments cannot construct a PotentialValue
    *  @throws InterruptedException if the current thread was interrupted while waiting on a Future
    *                               during the gradient norm calculation
    *  @throws ExecutionException if an exception is thrown by a thread during calculation of the 
    *                             gradient norms
    *  @throws CancellationException if one of the computations was cancelled during calculation of
    *                                the gradient norms
    *  @throws NullPointerException if X or field are null
    *  @throws RejectedExecutionException if a multithreaded task cannot be scheduled during 
    *                                     calculation of the gradient norms
    */
   public PotentialValue(IPotentialField field,
                         int[] subset,
                         double[][] X,
                         double[][] gradient,
                         double[] gradientNorms)
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException,
             IllegalArgumentException {
      if (field == null) throw new NullPointerException("potential field must not be null");
      if (X == null) throw new NullPointerException("coordinate matrix X must not be null");
      this.field = field;
      this.X = X;
      this.gradient = (gradient == null? new double[X.length][X[0].length] : gradient);
      if (this.gradient.length != X.length || this.gradient[0].length != X[0].length)
         throw new IllegalArgumentException("gradient is not the same size as X!");
      this.gradientNorms = (gradientNorms == null? new double[X[0].length] : gradientNorms);
      if (this.gradientNorms.length != X[0].length)
         throw new IllegalArgumentException("gradientNorms is not the same size as X[0]!");
      if (subset == null) {
         subset = new int[X[0].length];
         for (int i = 0; i < subset.length; ++i) subset[i] = i;
      }
      this.subset = subset;
      // okay, now we calculate...
      this.potential = field.calculate(subset, X, gradient);
      // now we calculate gradient norms and length...
      double glen;
      if (this.subset.length < Par.MIN_SUGGESTED_TASKS) {
         GradWorker gw = new GradWorker(0, 1);
         gw.run();
         glen = gw.gradlen2;
         this.steepestVertex = gw.maxgrad;
      } else {
         int i, n = Par.workers(), maxgradid;
         double tmp;
         GradWorker[] gw = new GradWorker[n];
         for (i = 0; i < n; ++i)
            gw[i] = new GradWorker(i, n);
         Par.run(gw);
         maxgradid = gw[0].maxgrad;
         tmp = this.gradientNorms[maxgradid];
         glen = gw[0].gradlen2;
         for (i = 1; i < n; ++i) {
            glen += gw[i].gradlen2;
            if (this.gradientNorms[gw[i].maxgrad] > tmp) {
               maxgradid = gw[i].maxgrad;
               tmp = this.gradientNorms[maxgradid];
            }
         }
         this.steepestVertex = maxgradid;
      }
      this.gradientLength = Math.sqrt(glen);
   }
   public PotentialValue(IPotentialField field,
                         int[] subset,
                         double[][] X,
                         double[][] gradient) {
      this(field, subset, X, gradient, null);
   }
   public PotentialValue(IPotentialField field,
                         int[] subset,
                         double[][] X,
                         double[] gradientNorms) {
      this(field, subset, X, null, gradientNorms);
   }
   public PotentialValue(IPotentialField field,
                         int[] subset,
                         double[][] X) {
      this(field, subset, X, null, null);
   }   public PotentialValue(IPotentialField field,
                         double[][] X,
                         double[][] gradient,
                         double[] gradientNorms) {
      this(field, null, X, gradient, gradientNorms);
   }
   public PotentialValue(IPotentialField field,
                         double[][] X,
                         double[][] gradient) {
      this(field, null, X, gradient, null);
   }
   public PotentialValue(IPotentialField field,
                         double[][] X,
                         double[] gradientNorms) {
      this(field, null, X, null, gradientNorms);
   }
   public PotentialValue(IPotentialField field,
                         double[][] X) {
      this(field, null, X, null, null);
   }

}

