////////////////////////////////////////////////////////////////////////////////////////////////////
// SchiraModel.java
//
// The nben.neuroscience namespace is intended to contain a mix of useful tools and models for doing
// calculations over neuroscience data such as fMRI data; it is designed to work with front-end 
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

package nben.neuroscience;

import nben.util.Par;
import nben.util.Numbers;

import java.util.Arrays;


/** The SchiraModel class constructs an instance of the banded double-sech model from the paper:
 *  Schira MM, Tyler CW, Spehar B, Breakspear M (2010) Modeling Magnification and Anisotropy in the
 *  Primate Foveal Confluence. PLOS Comput Biol 6(1):e1000651. doi:10.1371/journal.pcbi.1000651
 *  
 *  @author Noah C. Benson
 */
public class SchiraModel {

   // The parameters of the model
   public final double A;
   public final double B;
   public final double lambda;
   public final double psi;
   public final double v1Size;
   public final double v2Size;
   public final double v3Size;
   public final double hv4Size;
   public final double v3aSize;
   public final double xFC;
   public final double yFC;
   public final double xScale;
   public final double yScale;
   public final double xShear;
   public final double yShear;

   public final double sinPsi;
   public final double cosPsi;
   public final double FC0;

   /** Constructs a SchiraModel object
    */
   public SchiraModel(double _A,
                      double _B,
                      double _lambda,
                      double _psi,
                      double _v1Size,
                      double _v2Size,
                      double _v3Size,
                      double _hv4Size,
                      double _v3aSize,
                      double _xFC,
                      double _yFC,
                      double _xScale,
                      double _yScale,
                      double _xShear,
                      double _yShear) {
      A = _A;
      B = _B;
      lambda = _lambda;
      psi = _psi;
      v1Size = _v1Size;
      v2Size = _v2Size;
      v3Size = _v3Size;
      hv4Size = _hv4Size;
      v3aSize = _v3aSize;
      xFC = _xFC;
      yFC = _yFC;
      xScale = _xScale;
      yScale = _yScale;
      xShear = _xShear;
      yShear = _yShear;

      sinPsi = Math.sin(psi);
      cosPsi = Math.cos(psi);
      FC0 = Math.log((A + lambda) / (B + lambda));
   }

   // Constants:
   public final double DOUBLE_SECH_PARAM1 = 0.1821;
   public final double DOUBLE_SECH_PARAM2 = 0.76;

   // convert to/from standard mathematica coordinates to the theta/rho coordinates of visual angle
   private final double visualToMath(double angle) {return Math.PI/180.0 * (90.0 - angle);}
   private final double mathToVisual(double angle) {return 90.0 - 180.0/Math.PI * angle;}
   private final double[] cartToVisual(double[] xy) {
      double[] q = new double[2];
      q[0] = mathToVisual(Math.atan2(xy[1], xy[0]));
      q[1] = Math.sqrt(xy[0]*xy[0] + xy[1]*xy[1]);
      return q;
   }
   private final double[] visualToCart(double[] q) {
      double[] xy = new double[2];
      double th = visualToMath(q[0]);
      xy[0] = q[1] * Math.cos(th);
      xy[1] = q[1] * Math.sin(th);
      return xy;
   }

   public final static double sech(double x) {
      return 2.0 / (Math.exp(x) + Math.exp(-x));
   }

   // method for rotation/translation/shear
   private final void finalTx(double[] z) {
      double x, y;
      // center the point...
      x = z[0] - FC0;
      // flip the z so that it matches LH...
      y = -z[1];
      // shear...
      z[0] = x + xShear*y;
      z[1] = yShear*x + y;
      // rotate...
      x = z[0]*cosPsi - z[1]*sinPsi;
      y = z[0]*sinPsi + z[1]*cosPsi;
      // scale and re-center...
      z[0] = xFC + xScale*x;
      z[1] = yFC + yScale*y;
   }
   

   /** model.angleToCortex(theta, rho) yields a 5 x 2 double array in which each of the 5 columns is
    *  a double array of [x,y] which predicts the position of the given visual angle position 
    *  specifiedfor by the polar angle theta and the eccentricity rho for each of the visual areas
    *  (0) V1, (1) V2, (2) V3, (3) hV4, and (4) V3a.
    */
   public final double[][] angleToCortex(double theta, double rho) {
      double[][] xy = new double[5][2];
      // first, the layerless part...
      double zz = visualToMath(theta) * 2.0 / Math.PI;
      double abszz = Math.abs(zz);
      double sgn = (Numbers.zeroish(zz)? 1.0 : Math.signum(zz));
      rho = Math.abs(rho);
      // layered part...
      double[] layers = new double[5];
      layers[0] = v1Size * zz;
      layers[1] = sgn * (v1Size + v2Size*(1.0 - abszz));
      layers[2] = sgn * (v1Size + v2Size + v3Size*abszz);
      layers[3] = v1Size + v2Size + v3Size + hv4Size*(1.0 - 0.5*(zz + 1.0));
      layers[4] = -(v1Size + v2Size + v3Size + 0.5*v3aSize*(zz + 1.0));
      // log-polar part...
      double argz, absz, rz, iz, numth, divth, numr, numi, divr, divi;
      for (int i = 0; i < 5; ++i) {
         zz = layers[i];
         rz = rho * Math.cos(zz);
         iz = rho * Math.sin(zz);
         if (rz >= 0) rz += lambda;
         else rz += 2.0 * lambda * (1.0 - Math.abs(zz)/Math.PI);
         argz = Math.atan2(iz, rz);
         absz = Math.sqrt(iz*iz + rz*rz);
         if (Numbers.zeroish(absz)) {
            xy[i][0] = Math.log(A/B);
            xy[i][1] = 0.0;
         } else {
            numr = sech(argz);
            numth = argz * Math.pow(
              numr,
              DOUBLE_SECH_PARAM1 * sech(DOUBLE_SECH_PARAM2*Math.log(absz / A)));
            divth = argz * Math.pow(
              numr,
              DOUBLE_SECH_PARAM1 * sech(DOUBLE_SECH_PARAM2*Math.log(absz / B)));
            numr = A + absz * Math.cos(numth);
            divr = B + absz * Math.cos(divth);
            numi = absz * Math.sin(numth);
            divi = absz * Math.sin(divth);
            numth = Math.atan2(numi, numr);
            divth = Math.atan2(divi, divr);
            numr = Math.sqrt(numr*numr + numi*numi);
            divr = Math.sqrt(divr*divr + divi*divi);
            xy[i][0] = Math.log(numr / divr);
            xy[i][1] = numth - divth;
            if (xy[i][1] < -Math.PI) xy[i][1] += 2.0*Math.PI;
            else if (xy[i][1] > Math.PI) xy[i][1] -= 2.0*Math.PI;
         }
         finalTx(xy[i]);
      }
      return xy;
   }
   /** model.angleToCortex(thetas, rhos) yields an n-length double array q, each of whose elements,
    *  q[i], is equal to model.angleToCortex(thetas[i], rhos[i]).
    */
   public final double[][][] angleToCortex(double[] thetas, double[] rhos) {
      if (thetas.length != rhos.length)
         throw new IllegalArgumentException("arrays given to angleToCortex must be equally-sized");
      double[][][] res = new double[thetas.length][][];
      for (int i = 0; i < thetas.length; ++i)
         res[i] = angleToCortex(thetas[i], rhos[i]);
      return res;
   }
   /** model.angleToCortex(angles) yields an n-length double array q, each of whose elements, q[i],
    *  is equal to model.angleToCortex(thetas[0][i], rhos[1][i]). If the angles array is passed as
    *  an n x 2 matrix instead of a 2 x n matrix, it is automatically interpreted correctly.
    */
   public final double[][][] angleToCortex(double[][] angles) {
      if (angles.length == 2 && angles[0].length != 2) {
         if (angles[0].length != angles[1].length)
            throw new IllegalArgumentException("array given to angleToCortex must be a matrix");
         return angleToCortex(angles[0], angles[1]);
      } else {
         double[][][] res = new double[angles.length][][];
         for (int i = 0; i < angles.length; ++i)
            res[i] = angleToCortex(angles[i][0], angles[i][1]);
         return res;
      }
   }

   /** model.cortexToAngle(x, y) yields a length-3 double[] array a such that a[0] is the polar
    *  angle of the point (x,y) on the cortex, a[1] is the eccentricity, and a[3] is 1.0, 2.0,
    *  3.0, 4.0, or 5.0, indicating that (x,y) is in V1, V2, V3, hV4, or V3A, respectively (note
    *  that hV4 and V3A are not predictive --- they are pseudo-areas that stabilize registrations
    *  and should not be considered valid predictions). If the point (x,y) does not fall within the
    *  model, then null is yielded.
    */                      
   public final double[] cortexToAngle(double x, double y) {
      throw new UnsupportedOperationException("cortexToAngle not yet implemented");
   }
   /** model.cortexToAngle(x, y) yields an n x 3 double array in which each of the n rows
    *  corresponds to the n values in x and y, and in which each row i is equivalent to the output
    *  of model.cortexToAngle(x[i], y[i]).
    */                      
   public final double[][] cortexToAngle(double[] x, double[] y) {
      if (x.length != y.length)
         throw new IllegalArgumentException("arrays given to cortexToAngle must be equally-sized");
      double[][] res = new double[x.length][];
      for (int i = 0; i < res.length; ++i)
         res[i] = cortexToAngle(x[i], y[i]);
      return res;
   }
   /** model.cortexToAngle(xy) yields an n x 3 double array in which each of the n columns 
    *  corresponds to the n rows of xy, and in which each row i is equivalent to the output of
    *  model.cortexToAngle(xy[i][0], xy[i][1]). If the argument is a 2xn matrix instead of an nx2
    *  matrix, it is automatically transposed.
    */                      
   public final double[][] cortexToAngle(double[][] xy) {
      if (xy.length == 2 && xy[0].length != 2) {
         if (xy[0].length != xy[1].length)
            throw new IllegalArgumentException("array given to cortexToAngle must be a matrix");
         return cortexToAngle(xy[0], xy[1]);
      } else {
         double[][] res = new double[xy.length][];
         for (int i = 0; i < xy.length; ++i)
            res[i] = cortexToAngle(xy[i][0], xy[i][1]);
         return res;
      }
   }
 
}