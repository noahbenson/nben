////////////////////////////////////////////////////////////////////////////////////////////////////
// MeshPotential.java
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
import nben.geometry.R2.MeshTopology.Interpolator;
import nben.util.Num;
import nben.util.Par;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

/** The MeshPotential class allows one to define a potential field that is defined in terms of
 *  values on a triangle mesh; the mesh is interpolated linearly in order to produce potential
 *  values and gradients. 
 *
 *  @author Noah C. Benson
 */
public class MeshPotential extends ASimplexPotential {
   /** the mesh topology that tracks the points and triangles of the field */
   public final MeshTopology topology;
   /** the specific registration of the topology for the field mesh */
   public final MeshTopology.Registration registration;
   /** the values (d x n with d = number dimensions and n = number mesh points) */
   public final double[][] values;
   /** the dimension of the value for each simplex */
   public final int[] simplexValueDimensions;
   /** the polar angle values of the points being tracked */
   public final double[][] vertexValues;
   /** the private array of interpolation objects that gets updated every calculation */
   protected MeshTopology.Interpolator[] m_interps;

   // a simple function to turn a list of vertexID's into simplices for ASimplexPotential
   private static final int[][] vertexIDsToSimplices(int[] vertexIDs) {
      int[][] S = new int[1][vertexIDs.length * 2];
      int i, n = vertexIDs.length;
      for (i = 0; i < n; ++i) S[0][i]     = vertexIDs[i];
      for (i = 0; i < n; ++i) S[0][i + n] = vertexIDs[i];
      return S;
   }
   // private function for filling up an array of identical differentiated functions...
   private final static IDifferentiatedFunction[] fillDiffFns(IDifferentiatedFunction f, int n) {
      IDifferentiatedFunction[] fs = new IDifferentiatedFunction[n];
      for (int i = 0; i < n; ++i) fs[i] = f;
      return fs;
   }
   // private function for doubling an array of identical differentiated functions...
   private final static IDifferentiatedFunction[] doubleDiffFns(IDifferentiatedFunction[] f) {
      int i, n = f.length;
      IDifferentiatedFunction[] fs = new IDifferentiatedFunction[n * 2];
      for (i = 0; i < n; ++i) fs[i]     = f[i];
      for (i = 0; i < n; ++i) fs[i + n] = f[i];
      return fs;
   }
   
   /** MeshPotential overloads calculate so as to construct interp objects for each vertex in
    *  advance
    */
   synchronized public double calculate(double[][] X, double[][] G)
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException {
      // find all the interpolation points
      topology.pinterpolation(registration, X, subset, 1, true, m_interps);
      // now run the normal calculation, which will use these
      return super.calculate(X, G);
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
      int i, ii,
          u = simplices[0][id],
          dim = simplexValueDimensions[id];
      // first, extract the relevant point
      double[] pt  = new double[X.length], npt;
      for (i = 0; i < pt.length; ++i) pt[i] = X[i][u];
      // next, we want to find the interpolation coordinates for the nearest point
      MeshTopology.Interpolator interp = m_interps[u];
      // do the gradient if we have been passed a valid workspace
      // the normal vector to the triangle is the cross-product...
      if (interp == null || interp.nearest) {
         // not in a triangle -- gradient must be 0
         if (G != null) {
            G[0][0] = 0;
            G[0][1] = 0;
         }            
      } else if (G != null) {
         // the normal vector projected onto the 2D surface should give us the gradient
         double[] a = new double[3],
                  b = new double[3],
                  normal;
         int j, k;
         i = interp.indices[0];
         j = interp.indices[1];
         k = interp.indices[2];
         a[0] = registration.coordinates[j][0] - registration.coordinates[i][0];
         a[1] = registration.coordinates[j][1] - registration.coordinates[i][1];
         b[0] = registration.coordinates[k][0] - registration.coordinates[i][0];
         b[1] = registration.coordinates[k][1] - registration.coordinates[i][1];
         a[2] = values[dim][j] - values[dim][i];
         b[2] = values[dim][k] - values[dim][i];
         // the normal vector to the plane (z is the angle)
         normal = Num.cross(a, b);
         // let normal = (nx, ny, nz),
         //     r0     = normal . a
         // normal . (x,y,z) - r0 == 0
         // nx x + ny y + nz z - r0 == 0
         // z == (r0 - nx x - ny y)/nz
         // dz/dx == -nx/nz
         // dz/dy == -ny/nz
         G[0][0] = -normal[0] / normal[2];
         G[0][1] = -normal[1] / normal[2];
      }
      // return the values at this point
      if (interp == null) return 0;
      else return interp.interpolate(values[dim]);
   }

   /** MeshPotential(meshCoordinates, meshTriangles, meshAngles, meshEccens,
    *            vertexIndices, vertexAngles, vertexEccens, vertexWeights)
    *  constructs a new potential field consisting of a triangle mesh with angle and eccentricity
    *  values sampled at the given meshCoordinates; the vertexIndices detemine the vertices tracked
    *  and given gradient values by the potential, based on their angles and eccentricities.
    *
    *  @param f the differentiated functions that determine the shape of the potential
    *  @param meshCoordinates the N x 3 matrix of coordinates at which the function is sampled
    *  @param meshTriangles the N x 3 matrix of vertex indices (into meshCoordinates) that specify
    *                       the triangles in the mesh
    *  @param meshValues the D x N matrix of field values; D can be any number but must match
    *                    the size of vertexValues
    *  @param vertexIndices the M-length vector of indices of the vertices at which the potential
    *                       will be calculated
    *  @param vertexValues the D x M matrix of values for the vertices; D must match meshValues
    *  @param X0 the initial 2 x N matrix of vertex coordinates
    */
   public MeshPotential(IDifferentiatedFunction[] f,
                        double[][] meshCoordinates,
                        int[][] meshTriangles,
                        double[][] meshValues,
                        int[] vertexIndices,
                        double[][] vertexValues,
                        double[][] X0) {
      super(doubleDiffFns(f), vertexIDsToSimplices(vertexIndices), X0);
      topology = MeshTopology.from(meshTriangles);
      registration = topology.register(meshCoordinates);
      int i, j,
          d = meshValues.length,
          n = vertexIndices.length,
          m = registration.coordinates.length;
      if (d != vertexValues.length)
         throw new IllegalArgumentException("number of mesh and vertex value dims must match");
      for (i = 0; i < meshValues.length; ++i) {
         if (m != meshValues[i].length)
            throw new IllegalArgumentException("mesh values and coord count must be equal");
      }
      values = meshValues.clone();
      for (i = 0; i < vertexValues.length; ++i) {
         if (vertexValues[i].length != n)
            throw new IllegalArgumentException("vertex indices and values must be the same length");
      }
      this.vertexValues = vertexValues.clone();
      // the simplex reference values need to be edited
      for (j = 0; j < vertexValues.length; ++j) {
         for (i = 0; i < vertexIndices.length; ++i)
            M0[i + j*n] = vertexValues[j][i];
      }
      // make the dimension notes
      this.simplexValueDimensions = new int[n*2];
      for (i = 0; i < d; i++) {
         m = n*i;
         for (j = 0; j < n; ++j) this.simplexValueDimensions[j + m] = i;
      }
      // allocate our private data-slots
      this.m_interps = new MeshTopology.Interpolator[X0[0].length];
   }
   public MeshPotential(IDifferentiatedFunction f,
                        double[][] meshCoordinates,
                        int[][] meshTriangles,
                        double[][] meshValues,
                        int[] vertexIndices,
                        double[][] vertexValues,
                        double[][] X0) {
      this(fillDiffFns(f, 2*vertexIndices.length),
           meshCoordinates, meshTriangles, meshValues,
           vertexIndices, vertexValues, X0);
   }
   // for subfield'ing
   protected MeshPotential(MeshPotential field, int[] ss) {
      super(field, ss);
      this.topology = field.topology;
      this.registration = field.registration;
      this.vertexValues = field.vertexValues;
      this.values = field.values;
      this.simplexValueDimensions = field.simplexValueDimensions;
      this.m_interps = field.m_interps;
   }

   /** Yields a duplicate MeshPotential object but with the given subset of vertices.
    */
   public MeshPotential subfield(int[] ss) {return new MeshPotential(this, ss);}
}
