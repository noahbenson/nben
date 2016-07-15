////////////////////////////////////////////////////////////////////////////////////////////////////
// RetinotopyMeshField.java
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

import nben.geometry.R2.MeshTopology;
import nben.util.Num;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

/** The RetinotopyMeshField class allows one to define a potential field that is defined in terms of
 *  values on a triangle mesh; the mesh is interpolated linearly in order to produce potential
 *  values and gradients. 
 *
 *  @author Noah C. Benson
 */
public class RetinotopyMeshField extends ASimplexPotential {
   /** the mesh topology that tracks the points and triangles of the field */
   public final MeshTopology topology;
   /** the specific registration of the topology for the field mesh */
   public final MeshTopology.Registration registration;
   /** the polar angle or eccentricity values (one per vertex/coordinate) */
   public final double[] angles;
   /** the polar angle values of the points being tracked */
   public final double[] vertexAngles;

   // a simple function to turn a list of vertexID's into simplices for ASimplexPotential
   private static final int[][] vertexIDsToSimplices(int[] vertexIDs) {
      int[][] S = new int[1][];
      S[0] = vertexIDs;
      return S;
   }
   // private function for filling up an array of identical differentiated functions...
   private final static IDifferentiatedFunction[] fillDiffFns(IDifferentiatedFunction f, int n) {
      IDifferentiatedFunction[] fs = new IDifferentiatedFunction[n];
      for (int i = 0; i < n; ++i) fs[i] = f;
      return fs;
   }

   /** retinoPotential.calculateSimplex(id, X, G) calculates the potential difference based on the
    *  given id by looking up the X coordinaets in the given coordinate matrix (size: dims x
    *  vertices) X. If the final argument G is non-null, it places the gradient value in the
    *  appropriate entries of the (1 x dims)-sized matrix G.
    */
   public final double calculateSimplex(int id, double[][] X, double[][] G) {
      // if we haven't initialized anything yet, we return 0;
      // this only happens during the calls to this function that are run during the constructor of
      // ASimplexPotential. We want to replace these with the reference value in the constructor
      // anyway:
      if (topology == null) return 0.0;
      int i, u = simplices[0][id];
      // first, extract the relevant point
      double[] pt = new double[X.length];
      for (i = 0; i < pt.length; ++i) pt[i] = X[i][u];
      // next, we want to find the interpolation coordinates for the nearest point
      MeshTopology.Interpolator interp = topology.interpolate(registration, pt, 1, true); //#check
      // the interp object can be used for angle; do this first:
      double predictAngle = interp.interpolate(angles);
      // do the gradient if we have been passed a valid workspace
      if (G != null) {
         // the normal vector to the triangle is the cross-product...
         if (interp.indices.length != 3) {
            // not in a triangle -- gradient must be 0
            G[0][0] = 0;
            G[0][1] = 0;
         } else {
            // the normal vector projected onto the 2D surface should give us the gradient
            double[] a = new double[3],
                     b = new double[3],
                     cx;
            int j, k;
            i = interp.indices[0];
            j = interp.indices[1];
            k = interp.indices[2];
            a[0] = topology.coordinates[j][0] - topology.coordinates[i][0];
            a[1] = topology.coordinates[j][1] - topology.coordinates[i][1];
            b[0] = topology.coordinates[k][0] - topology.coordinates[i][0];
            b[1] = topology.coordinates[k][1] - topology.coordinates[i][1];
            cx = Num.cross(a, b);
            // the math for this ends up working out however the cross product is scaled:
            G[0][0] = -cx[0] / cx[2];
            G[0][1] = -cx[1] / cx[2];
         }
      }
      return predictAngle;
   }

   /** RetinotopyMeshField(meshCoordinates, meshTriangles, meshAngles, meshEccens,
    *                      vertexIndices, vertexAngles, vertexEccens, vertexWeights)
    *  constructs a new potential field consisting of a triangle mesh with angle and eccentricity
    *  values sampled at the given meshCoordinates; the vertexIndices detemine the vertices tracked
    *  and given gradient values by the potential, based on their angles and eccentricities.
    *
    *  @param f the differentiated functions that determine the shape of the potential
    *  @param meshCoordinates the N x 3 matrix of coordinates at which the function is sampled
    *  @param meshTriangles the N x 3 matrix of vertex indices (into meshCoordinates) that specify
    *                       the triangles in the mesh
    *  @param meshAngles the N-length vector of visual angle values at each vertex in the mesh
    *  @param vertexIndices the M-length vector of indices of the vertices at which the potential
    *                       will be calculated
    *  @param vertexAngles the M-length vector of visual angle values of each of the vertices
    *  @param X0 the initial 2 x N matrix of vertex coordinates
    */
   public RetinotopyMeshField(IDifferentiatedFunction[] f,
                              double[][] meshCoordinates,
                              int[][] meshTriangles,
                              double[] meshAngles,
                              int[] vertexIndices,
                              double[] vertexAngles,
                              double[][] X0) {
      super(f, vertexIDsToSimplices(vertexIndices), X0);
      topology = MeshTopology.from(meshTriangles);
      registration = topology.register(meshCoordinates);
      if (registration.coordinates.length != meshAngles.length)
         throw new IllegalArgumentException("mesh angles, eccens, and coord count must be equal");
      angles = meshAngles.clone();
      if (vertexAngles.length != vertexIndices.length)
         throw new IllegalArgumentException(
           "vertex indices, angles, and eccens must be the same length");
      vertexAngles = vertexAngles.clone();
      // the simplex reference values need to be edited
      for (int i = 0; i < vertexIndices.length; ++i)
         M0[i] = angles[i];
   }
   public RetinotopyMeshField(IDifferentiatedFunction f,
                              double[][] meshCoordinates,
                              int[][] meshTriangles,
                              double[] meshAngles,
                              int[] vertexIndices,
                              double[] vertexAngles,
                              double[][] X0) {
      this(fillDiffFns(f, vertexIndices.length), meshCoordinates, meshTriangles, meshAngles,
           vertexIndices, vertexAngles, X0);
   }
   // for subfield'ing
   protected RetinotopyMeshField(RetinotopyMeshField field, int[] ss) {
      super(field, ss);
      this.topology = field.topology;
      this.registration = field.registration;
      this.vertexAngles = field.vertexAngles;
      this.angles = field.angles;
   }

   /** Yields a duplicate RetinotopyMeshField object but with the given subset of vertices.
    */
   public RetinotopyMeshField subfield(int[] ss) {return new RetinotopyMeshField(this, ss);}
}
