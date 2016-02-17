////////////////////////////////////////////////////////////////////////////////////////////////////
// AnglePotential.java
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
import nben.neuroscience.registration.ASimplexPotential;
import nben.neuroscience.registration.IDifferentiatedFunction;

import nben.util.Numbers;

/** The AnglePotential class defines the code computations of potential fields based on the
 *  interactions between triples of neighboring vertices in the mesh. To construct an 
 *  AnglePotential, one must explicitly or implicitly choose a differentiated function f; the
 *  potential calculated by the edge potential class will then be the value f(a, a0) where a and a0
 *  are the angles and the reference angles of the mesh, respectively.
 *
 *  @author Noah C. Benson
 */
public class AnglePotential extends ASimplexPotential {
   
   /** anglePotential.calculateAngle(id, X, G) calculates the angle labeled by the given id,
    *  looking up the X coordinaets in the given coordinate matrix (size dims x vertices) X.
    *  If the final argument G is non-null, it places the gradient value in the appropriate
    *  entries of the (3 x dims)-sized matrix G.
    *
    *  @returns the value of angle id at the position X
    */
   public final double calculateSimplex(int id, double[][] X, double[][][] G) {
      double abx, aby, acx, acy, theta;
      int i, dims = X.length;
      double[] nAB = new double[dims];
      double[] nAC = new double[dims];
      double dAC, dAB;
      if (dims == 2) {
         abx = X[0][simplices[1][id]] - X[0][simplices[0][id]];
         aby = X[1][simplices[1][id]] - X[1][simplices[0][id]];
         acx = X[0][simplices[2][id]] - X[0][simplices[0][id]];
         acy = X[1][simplices[2][id]] - X[1][simplices[0][id]];
         theta = Math.atan2(acy, acx) - Math.atan2(aby, abx);
         dAB = Math.sqrt(abx*abx + aby*aby);
         nAB[0] = abx/dAB;
         nAB[1] = aby/dAB;
         dAC = Math.sqrt(acx*acx + acy*acy);
         nAC[0] = acx/dAC;
         nAC[1] = acy/dAC;
      } else {
         // we have to convert to a flattened 2d form...
         int A = simplices[0][id];
         int B = simplices[1][id];
         int C = simplices[2][id];
         double[] axisZ = new double[3];
         double tmp = Math.sqrt(X[0][A]*X[0][A] + X[1][A]*X[1][A] + X[2][A]*X[2][A]);
         axisZ[0] = X[0][A]/tmp;
         axisZ[1] = X[1][A]/tmp;
         axisZ[2] = X[2][A]/tmp;
         nAB[0] = X[0][B] - X[0][A];
         nAB[1] = X[1][B] - X[1][A];
         nAB[2] = X[2][B] - X[2][A];
         dAB = Math.sqrt(nAB[0]*nAB[0] + nAB[1]*nAB[1] + nAB[2]*nAB[2]);
         nAB[0] /= dAB;
         nAB[1] /= dAB;
         nAB[2] /= dAB;
         double[] axisY = Numbers.cross(axisZ, nAB);
         tmp = Math.sqrt(axisY[0]*axisY[0] + axisY[1]*axisY[1] + axisY[2]*axisY[2]);
         axisY[0] /= tmp;
         axisY[1] /= tmp;
         axisY[2] /= tmp;
         double[] axisX = Numbers.cross(axisY, axisZ);
         // now we can get the 2D coordinates out...
         nAC[0] = X[0][C] - X[0][A];
         nAC[1] = X[1][C] - X[1][A];
         nAC[2] = X[2][C] - X[2][A];
         acx = nAC[0]*axisX[0] + nAC[1]*axisX[1] + nAC[2]*axisX[2];
         acy = nAC[0]*axisY[0] + nAC[1]*axisY[1] + nAC[2]*axisY[2];
         dAC = Math.sqrt(nAC[0]*nAC[0] + nAC[1]*nAC[1] + nAC[2]*nAC[2]);
         nAC[0] /= dAC;
         nAC[1] /= dAC;
         nAC[2] /= dAC;
         // ab is lined up with the x-axis
         aby = 0.0;
         abx = dAB * (nAB[0]*axisX[0] + nAB[1]*axisX[1] + nAB[2]*axisX[2]);
         // and theta...
         theta = Math.atan2(acy, acx) - Math.atan2(aby, abx);
      }
      // make sure the vectors didn't cross the -x axis
      if (theta < -Math.PI) theta += 2.0*Math.PI;
      else if (theta > Math.PI) theta -= 2.0*Math.PI;
      // set G to the gradient of theta in terms of X
      if (G != null) {
         double cos = Math.cos(theta);
         double sin = Math.sqrt(1.0 - cos*cos);
         double[] g0 = G[0];
         double[] g1 = G[1];
         double[] g2 = G[2];
         for (i = 0; i < dims; ++i) {
            // start with corner 1:
            g1[i] = (cos * nAB[i] - nAC[i]) / (sin*dAB);
            // then corner 2:
            g2[i] = (cos * nAC[i] - nAB[i]) / (sin*dAC);
            // finally corner 0:
            g0[i] = -(g1[i] + g2[i]);
         }
      }
      return theta;
   }

   /** Constructs an AnglePotential object.
    *  
    *  @param f an IDifferentiatedFunction object indicating the form of the potential landscape
    *  @param faces the (3 x n) list of trisimplices; must be 0-indexed and must have trisimplices listed
    *               all in the same ordering (clockwise or counter-clockwise)
    *  @param X0 a (dims x n) array of the starting coordinates of the vertices
    */
   public AnglePotential(IDifferentiatedFunction[] f, int[][] simplices, double[][] X0) {
      super(f, simplices, X0);
   }
   public AnglePotential(IDifferentiatedFunction f, int[][] simplices, double[][] X0) {
      super(f, simplices, X0);
   }
   protected AnglePotential(AnglePotential field, int[] ss) {
      super(field, ss);
   }
   /** Yields a duplicate AnglePotential object but with the given subset of vertices.
    */
   public AnglePotential subfield(int[] ss) {return new AnglePotential(this, ss);}
}