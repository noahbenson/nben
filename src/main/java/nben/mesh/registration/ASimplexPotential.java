////////////////////////////////////////////////////////////////////////////////////////////////////
// ASimplexPotential.java
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

import nben.mesh.registration.Util;
import nben.mesh.registration.IPotentialField;
import nben.mesh.registration.IDifferentiatedFunction;

import nben.util.Par;
import nben.util.Numbers;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

import java.util.HashSet;
import java.util.Iterator;

/** The ASimplexPotential class defines the code that computates of potential fields based on the
 *  interactions between (usually small) submeshes in the mesh. Examples of these include edge,
 *  angle, and dihedral potentials. This abstract class implements data structures common to all of
 *  these.
 *  In order to successfully reify ASimplexPotential, a class must implement these methods:
 *  (1) calculateSimplex, which must return the measure of any one simplex, (2) subfield, which must
 *  return a new potential field via the constructor ASimplexPotential(oldField, newSubset).
 *
 *  @author Noah C. Benson
 */
public abstract class ASimplexPotential implements IPotentialField {
   /** The subset over which this particular simplex potential calculates.
    */
   protected final int[] subset;
   /** The subset of simplices over which this particular simplex potential calculates.
    */
   protected final int[] simplexSubset;
   /** The simplex member variable stores the simplices tracked by the field. This will always be
    *  a (dims x simplces)-matrix where dims is the number of vertices in each simplex.
    */
   protected final int[][] simplices;
   /** p.simplexCount() yields the number of simplices in the potential field p.
    *
    *  @return the number of simplices tracked by the potential field
    */
   public final int simplexCount() {return simplices[0].length;}
   /** p.simplexDimensions() yields the number of vertices in each simplex.
    *
    *  @return the number of vertices in each simplex
    */
   public final int simplexDimensions() {return simplices.length;}
   /** p.simplex(i) yields the i'th simplex tracked by the potential field p.
    *
    *  @param i the index of the simplex to return
    *  @return the simplex
    */
   public final int[] simplex(int i) {
      int[] s = new int[simplices.length];
      for (int k = 0; k < s.length; ++k)
         s[k] = simplices[k][i];
      return s;
   }

   /** The simplexIndex member variable keeps an index, one per vertex, of that vertex's simplices:
    *  for any vertex u, simplexIndex[u] = S such that S[k] is a simplex containing u for all valid
    *  k.
    */
   protected final int[][] simplexIndex;
   /** p.simplicesOf(u) yields a list of all the simplices containing vertex u.
    *
    *  @param u the vertex to look up
    *  @return a list of all the simplices containing vertex u.
    */
   public final int[] simplicesOf(int u) {
      return (simplexIndex[u] == null? null : simplexIndex[u].clone());
   }
   
   /** The simplexPosition member variable keeps an index, like simplexIndex, such that if
    *  if simplexIndex[i][j] gives the simplex id s such that one of the vertices of s is i,
    *  then simplexPosition[i][j] gives the simplex position k such that simplex[s][k] == i.
    */
   protected final int[][] simplexPosition;
   /** p.simplexPositionsOf(u) yields a list of all the simplex positions for the simplices
    *  containing vertex u.
    *
    *  @param u the vertex to look up
    *  @return a list of all the simplices containing vertex u.
    */
   public final int[] simplicesPositionsOf(int u) {
      return (simplexPosition[u] == null? null : simplexPosition[u].clone());
   }
   

   /** The member variable forms holds an array of IDifferentiatedFunction objects, each of which
    *  encapsulates the form or shape of one of the simplices tracked by the potential field.
    *  Note that it is 'safe' to change this variable as long as you manage the consequences of 
    *  multi-threading yourself; this variable is public in case the user wishes to, for
    *  example, change a parameter slowly during a minimization.
    */
   public final IDifferentiatedFunction[] forms;

   /** The M0 member variable stores the reference measurements of each simplex. This variable is 
    *  'safe' to change so long as you manage multiple-threads yourself; i.e., this variable is 
    *  public in case the user wishes to, for example, change the reference length of all edges
    *  slowly during a minimization.
    */
   public final double[] M0;

   /** p.calculateSimplex(i, X, W) is an abstract function that must yield the measure of the given
    *  simplex i in the potential field p given the coordinate matrix X; the gradient direction is
    *  to be stored in the (s x d)-sized workspace matrix W where s is the dimensionality of the
    *  simplices, and d is the dimensionality of the coordinate matrix. I.e., W[k][j] should be set
    *  to the j'th coordinate k'th vertex of the simplex i.
    *
    *  @param i the simplex number whose value is to be calculated and returned
    *  @param X the coordinate matrix at which to calculate the simplex's value
    *  @param W the workspace into which to write the simplex's gradient's direction vector
    *  @return the measure of the simplex at the coordinate matrix X
    */
   protected abstract double calculateSimplex(int i, double[][] X, double[][] W);

   /** workspace is a protected variable that holds the workspace for the given calculation. This
    *  needn't be accessed by derived classes, as the workspace is always passed to the 
    *  calculateSimplex function, but it is made available for flexibility.
    *  The workspace variable is always of size (m x s x d) where m is the the number of simplices,
    *  s is the number of vertices in a simplex, and d is the number of dimensions of the metric
    *  space over which the potential field operates.
    *
    *  @see calculateSimplex
    */
   protected final double[][][] workspace;
   /** The M member variable stores the measurements of each simplex during a calculation. This
    *  variable is protected to allow access to it by derived classes purely for flexibility. It 
    *  should otherwise be considered similar to the workspace variable.
    */
   protected final double[] M;
   /** The simplexPotential member variable stores the simplex potential measurements of each
    *  simplex during a calculation. This variable is protected to allow access to it by derived
    *  classes purely for flexibility. It should otherwise be considered similar to the workspace
    *  variable.
    */
   protected final double[] simplexPotential;

   // these are for convenience/use with the edge worker classes below
   private final int[] allVertices;
   private final int[] allSimplices;
   // if given just one differentiated function, fill up an array of it
   private static final IDifferentiatedFunction[] fillForms(IDifferentiatedFunction f, int n) {
      IDifferentiatedFunction[] fs = new IDifferentiatedFunction[n];
      for (int i = 0; i < n; ++i) fs[i] = f;
      return fs;
   }

   // this class calculates the first stage (fill in AB and edat) -- operate over edges
   private final class SimplexWorker implements Runnable {
      final int startID;  // the id we start with for this particular worker
      final int workers;  // the number of workers total
      double[][] X;       // the coordinate matrix at which we calculate
      public SimplexWorker(int myid, int nworkers) {
         this.startID = myid;
         this.workers = nworkers;
         this.X = null;
      }
      public void run() {
         int i, j, k, s;
         double mag, m, m0;
         double[][] ws;
         IDifferentiatedFunction f;
         for (i = startID; i < simplexSubset.length; i += workers) {
            s = simplexSubset[i];
            f = forms[s];
            ws = workspace[s];
            m0 = M0[s];
            // calculate the measure and gradient for this simplex
            M[s] = m = calculateSimplex(s, X, ws);
            // find the gradient length and scale the workspace accordingly
            mag = f.dy(m, m0);
            for (j = 0; j < ws.length; ++j) {
               for (k = 0; k < X.length; ++k)
                  ws[j][k] *= mag;
            }
            // now store the potential value in simplexPotential
            simplexPotential[s] = f.y(m, m0);
         }
      }
   }
   // this class calculates the second stage (fill in G and PE) -- operate over vertices
   private final class VertexWorker implements Runnable {
      final int startID;   // the id we start with for this particular worker
      final int workers;   // the number of workers total
      double[][] X;        // the coordinate matrix at which we calculate
      double potential;    // the total PE calculated across vertices in ths worker
      double[][] gradient; // the matrix in which we store the gradient
      public VertexWorker(int myid, int nworkers) {
         this.startID = myid;
         this.workers = nworkers;
         this.potential = 0;
         this.X = null;
         this.gradient = null;
      }
      public void run() {
         int i, j, k, u, s;
         int[] idx1, idx2;
         double tmp, frac = 1.0 / simplices.length;
         double[] ws;
         potential = 0;
         for (i = startID; i < subset.length; i += workers) {
            u = subset[i];
            idx1 = simplexIndex[u];
            if (idx1 == null) continue;
            idx2 = simplexPosition[u];
            // sum u's simplices
            for (j = 0; j < idx1.length; ++j) {
               s = idx1[j];
               ws = workspace[s][idx2[j]];
               potential += frac * simplexPotential[s];
               for (k = 0; k < gradient.length; ++k)
                  gradient[k][u] += ws[k];
            }
         }
      }
   }
   // workers for this potential field
   private SimplexWorker[] simplexWorkers;
   private VertexWorker[] vertexWorkers;

   /** Constructs an ASimplexPotential object.
    *  
    *  @param f an array of IDfferentiatedFunction's that specify the shape of the potential 
    *           landscape for each simplex; this array is cloned.
    *  @param S the (s x m) list of simplices where s is the number of vertices in each simplex
    *           and m is the number of simplices; this will be cloned
    *  @param X0 a (d x n) array of the starting coordinates of the vertices where d is the number
    *            dimensions of each vertex coordinate and n is the number of vertices; this will be
    *            copied before being stored
    */
   public ASimplexPotential(IDifferentiatedFunction[] f, int[][] S, double[][] X0) {
      int i, j, k, q;
      int[] idx;
      if (S == null)
         throw new NullPointerException("Simplex matrix parameter S must not be null");
      if (S.length < 1)
         throw new IllegalArgumentException("Simplex matrix parameter S must not be empty");
      if (f == null)
         throw new NullPointerException("Form array parameter f must not be null");
      if (f.length != S[0].length)
         throw new IllegalArgumentException("forms f and simplex matrix must be equivalent sizes");
      if (X0 == null)
         throw new NullPointerException("Coordinate matrix parameter X0 must not be null");
      if (X0.length < 1)
         throw new IllegalArgumentException("Coordinate matrix parameter X0 must not be empty");
      this.forms = f;
      int s = S.length;
      int m = S[0].length;
      int d = X0.length;
      int n = X0[0].length;
      this.simplices = new int[s][m];
      // copy the array...
      for (j = 0; j < s; ++j) {
         if (S[j].length != m)
            throw new IllegalArgumentException("simplex matrix must not be ragged");
         System.arraycopy(S[j], 0, this.simplices[j], 0, m);
      }
      // build the index...
      this.simplexIndex = ASimplexPotential.buildSimplexIndex(n, S);
      this.simplexPosition = new int[n][];
      for (i = 0; i < n; ++i) {
         idx = simplexIndex[i];
         if (idx == null) continue;
         this.simplexPosition[i] = new int[idx.length];
         for (j = 0; j < idx.length; ++j) {
            q = idx[j];
            for (k = 0; k < S.length; ++k) {
               if (S[k][q] == i) {
                  this.simplexPosition[i][j] = k;
                  break;
               }
            }
         }
      }
      // allocate our workspace and simplex potential storage
      this.workspace = new double[m][s][d];
      this.simplexPotential = new double[m];
      // save the original distances
      this.M0 = new double[m];
      this.M  = new double[m];
      for (i = 0; i < m; ++i)
         M0[i] = calculateSimplex(i, X0, this.workspace[i]);
      // fill these in for convenience
      this.allSimplices = new int[m];
      for (i = 0; i < m; ++i) allSimplices[i] = i;
      this.allVertices = new int[n];
      for (i = 0; i < n; ++i) allVertices[i] = i;
      // the initial subset for any potential field is all vertices
      this.subset = allVertices;
      this.simplexSubset = allSimplices;
      // finally, make our workers
      int nwork = Par.workers();
      this.simplexWorkers = new SimplexWorker[nwork];
      this.vertexWorkers = new VertexWorker[nwork];
      for (i = 0; i < nwork; ++i) {
         this.simplexWorkers[i] = new SimplexWorker(i, nwork);
         this.vertexWorkers[i] = new VertexWorker(i, nwork);
      }
   }
   public ASimplexPotential(IDifferentiatedFunction f, int[][] S, double[][] X0) {
      this(ASimplexPotential.fillForms(f, S[0].length), S, X0);
   }

   /** ASimplexPotential(field, subset) constructs a simplex potential field object from the given
    *  field that operates over only the given subset of vertices; if subset is null that is taken
    *  to mean all vertices. This constructor should be called by any constructor designed to work
    *  with the subfield function. Generally the definition of subfield would be something like:
    *  SpecialField subfield(int[] ss) {return new SpecialField(this, ss);}
    *  SpecialField(SpecialField f, int[] ss) {super(f, ss); ...}
    *
    *  @param field the simplex potential field on which to base the new field
    *  @param ss the subset of vertices for the new potential field to use
    */
   protected ASimplexPotential(ASimplexPotential field, int[] ss) {
      this.subset           = (ss == null? field.allVertices : ss);
      this.simplexSubset    = (ss == null? field.allSimplices 
                                         : subsampleSimplexIndex(ss, field.simplices));
      this.forms            = field.forms;
      this.simplices        = field.simplices;
      this.simplexIndex     = field.simplexIndex;
      this.simplexPosition  = field.simplexPosition;
      this.workspace        = field.workspace;
      this.simplexPotential = field.simplexPotential;
      this.M0               = field.M0;
      this.M                = field.M;
      this.allSimplices     = field.allSimplices;
      this.allVertices      = field.allVertices;
      // allocate new workers
      int nwork = Par.workers();
      this.simplexWorkers = new SimplexWorker[nwork];
      this.vertexWorkers = new VertexWorker[nwork];
      for (int i = 0; i < nwork; ++i) {
         this.simplexWorkers[i] = new SimplexWorker(i, nwork);
         this.vertexWorkers[i] = new VertexWorker(i, nwork);
      }
   }

   /** Calculates the potential value and gradient at the given position X and places the gradient
    *  in the param G. When adding up the potential, each vertex in subset adds 1/s of the potential
    *  of each of its simplices where s is the number of vertices in a simplex; this means that if a
    *  vertex on one side of a simplex is in the subset but the other vertex of the simplex is not,
    *  then only half of that simplex's potential will be added to the total potential value.
    *
    *  @see IPotentialField
    */
   synchronized public double calculate(double[][] X, double[][] G)
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException {
      int i;
      int workers = vertexWorkers.length;
      double potential = 0;
      if (G == null) G = new double[X.length][X[0].length];
      for (i = 0; i < workers; ++i) {
         simplexWorkers[i].X = X;
         vertexWorkers[i].X = X;
         vertexWorkers[i].gradient = G;
      }
      // run, in parallel, the simplex calculations first; this fills up workspace
      Par.run(simplexWorkers);
      // now run, in parallel, the vertex calculatios; this fills up the gradient
      Par.run(vertexWorkers);
      // finally, sum up the vertex workers' potential values to return
      for (i = 0; i < workers; ++i) {
         if (Double.isNaN(vertexWorkers[i].potential) 
             || Double.isInfinite(vertexWorkers[i].potential))
            return vertexWorkers[i].potential;
         potential += vertexWorkers[i].potential;
      }
      return potential;
   }

   /** ASimplexPotential.buildSimplexIndex(n, S) yields an index idx for the given simplices in S
    *  such that idx[i] is an array of indices such that for any valid j, S[k][idx[i][j]] is equal
    *  to i for some valid k. The parameter n must be the number of vertices in the simplex list.
    *
    *  @param n number of vertices in the mesh that the simplex matrix S describes
    *  @param S the simplex matrix; this is expected to be an s x m matrix where s is the 
    *         dimensionality of the simplices and m is the number of simplices; i.e., for 20 edge
    *         simplices, S should be size 2 x 20; for 40 triangle simplices, S should be 3 x 40
    *  @return an index idx of size n such that idx[i] is an array of the simplex indices into S
    *          at which the value i can be found
    */
   public static int[][] buildSimplexIndex(int n, int[][] S) {
      int[][] index = new int[n][];
      int m = S[0].length;
      int[] u, v;
      for (int j = 0; j < S.length; ++j) {
         for (int i = 0; i < m; ++i) {
            u = index[S[j][i]];
            if (u == null) {
               u = new int[1];
               u[0] = i;
            } else {
               v = new int[u.length + 1];
               System.arraycopy(u, 0, v, 0, u.length);
               v[u.length] = i;
               u = v;
            }
            index[S[j][i]] = u;
         }
      }
      return index;
   }

   /** ASimplexPotential.subsampleSimplexIndex(subset, index) yields a set of all integers in the
    *  given index that are pointed to by any element of the given subset.
    *
    *  @param subset the subset of ids in index that should be subsampled
    *  @param index the index that should be subsampled from
    *  @return an array ss such that for each element of ss, ss[i], there is some k such that
    *          index[k, ss[i]] is an element of subset
    */
   public static int[] subsampleSimplexIndex(int[] subset, int[][] index) {
      int i,j;
      int[] ss;
      HashSet<Integer> q = new HashSet<Integer>();
      for (i = 0; i < subset.length; ++i) {
         ss = index[subset[i]];
         if (ss != null) {
            for (j = 0; j < ss.length; ++j) 
               q.add(new Integer(ss[j]));
         }
      }
      int[] samp = new int[q.size()];
      i = 0;
      for (Iterator<Integer> it = q.iterator(); it.hasNext(); ++i)
         samp[i] = it.next().intValue();
      return samp;
   }

}