////////////////////////////////////////////////////////////////////////////////////////////////////
// AnchorPotential.java
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
import nben.mesh.registration.ASimplexPotential;
import nben.mesh.registration.IDifferentiatedFunction;

/** The AnchorPotential class defines the code computations of potential fields based on the
 *  interactions between a vertex's position in the mesh and a fixed point in space. To construct an
 *  AnchorPotential, one must explicitly or implicitly choose a differentiated function f; the
 *  potential calculated by the anchor potential class will then be the value f(r, r0) where r and
 *  r0 are the distance from the vertex to the fixed point and the reference distance, respectively.
 *  Note that this class does not currently support a reference value (all references are 0) for the
 *  the IDifferentiatedFunction form of the potential; this may change in the future.
 *  Note that the AnchorPotential class is an instance of a simplex potential; anchors are
 *  considered 1-vertex simplices.
 *
 *  @author Noah C. Benson
 */
class AnchorPotential extends ASimplexPotential {
   /** The anchors member variable stores the positions to which the vertices tracked by this
    *  potential field are attracted. The size of this matrix is (d x q) where d is the 
    *  dimensionality of the embedding space and q is the number of anchors.
    */
   protected final double[][] anchors;

   // a simple function to turn a list of vertexID's into simplices for ASimplexPotential
   private static final int[][] vertexIDsToSimplices(int[] vertexIDs) {
      int[][] S = new int[1][];
      S[0] = vertexIDs;
      return S;
   }

   /** anchorPotential.calculatesimplex(id, X, G) calculates the anchor distance based on the
    *  given id by looking up the X coordinaets in the given coordinate matrix (size: dims x
    *  vertices) X. If the final argument G is non-null, it places the gradient value in the
    *  appropriate entries of the (1 x dims)-sized matrix G.
    *
    *  @returns the value of anchor id at the position X (i.e., the distance of the vertex from
    *           its anchor point)
    */
   public final double calculateSimplex(int id, double[][] X, double[][] G) {
      // if we haven't initialized anchors yet, we return 0; this only happens during the calls
      // to this function that are run during the constructor of ASimplexPotential. We want all
      // of these to return 0 anyway:
      if (anchors == null) return 0.0;
      int u = simplices[0][id], j;
      double[] g = G[0];
      double d = 0, tmp;
      for (j = 0; j < X.length; ++j) {
         g[j] = tmp = X[j][u] - anchors[j][id];
         d += tmp*tmp;
      }
      d = Math.sqrt(d);
      if (d > 0)
         for (j = 0; j < g.length; ++j) g[j] /= d;
      return d;
   }
   // private function for filling up an array of identical differentiated functions...
   private final static IDifferentiatedFunction[] fillDiffFns(IDifferentiatedFunction f, int n) {
      IDifferentiatedFunction[] fs = new IDifferentiatedFunction[n];
      for (int i = 0; i < n; ++i) fs[i] = f;
      return fs;
   }

   /** Constructs an AnchorPotential object.
    *  
    *  @param f an IDfferentiatedFunction that specifies the shape of the potential landscape
    *  @param ids an array of the indices of the vertices that are drawn toward an anchor point
    *  @param anchors a (dims x m) array of the anchor points for the given vertices.
    *  @param X0 a (dims x n) array of the starting coordinates of the vertices
    */
   public AnchorPotential(IDifferentiatedFunction[] f, 
                          int[] vertexIDs, double[][] anchorsX,
                          double[][] X0) {
      // Start by invoking the simplex constructor; this calls the calculateSimplex function for
      // each simplex (vertex ID). Because anchors is not yet set (should be null), this function
      // always returns 0, which is correct since the reference should be 0. After we have added
      // the anchors variable below, the calculateSimplex function should return an actual distance
      super(f, vertexIDsToSimplices(vertexIDs), X0);
      // do some sanity checking...
      if (anchorsX == null)
         throw new NullPointerException("Anchors argument must not be null");
      if (anchorsX.length != X0.length)
         throw new IllegalArgumentException("Anchor dims do not match those of coordinates");
      if (anchorsX[0].length != vertexIDs.length)
         throw new IllegalArgumentException("Anchor dims do not match those of coordinates");
      // now, we save the anchors...
      anchors = new double[X0.length][vertexIDs.length];
      for (int i = 0; i < X0.length; ++i)
         System.arraycopy(anchorsX[i], 0, anchors[i], 0, vertexIDs.length);
   }
   public AnchorPotential(IDifferentiatedFunction f,
                          int[] vertexIDs, double[][] anchorsX,
                          double[][] X0) {
      this(fillDiffFns(f, vertexIDs.length), vertexIDs, anchorsX, X0);
   }
   // for subfield'ing
   protected AnchorPotential(AnchorPotential field, int[] ss) {
      super(field, ss);
      this.anchors = field.anchors;
   }

   /** Yields a duplicate AnchorPotential object but with the given subset of vertices.
    */
   public AnchorPotential subfield(int[] ss) {return new AnchorPotential(this, ss);}
}
