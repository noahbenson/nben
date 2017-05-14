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
import nben.util.Num;

import java.util.Arrays;


/** The SchiraModel class constructs an instance of the banded double-sech model from the paper:
 *  Schira MM, Tyler CW, Spehar B, Breakspear M (2010) Modeling Magnification and Anisotropy in the
 *  Primate Foveal Confluence. PLOS Comput Biol 6(1):e1000651. doi:10.1371/journal.pcbi.1000651
 *  
 *  @author Noah C. Benson
 */
public class SchiraModel {

   // The max eccentricity of the model:
   public final double MAX_ECCENTRICITY = 90.0;

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
   public final double shearDiv;
   public final double v2Bound;
   public final double v3Bound;
   public final double v3aBound;
   public final double hv4Bound;


   // This private variable is used by the search routine in cortexToAngle;
   // because I have no simple inverse to the Schira Model, I employ a numeric
   // search to find the inverse.
   private final double[][] searchStartAngles;
   private final double[][][] searchStartPoints;
   private final static int nEccenPoints = 10;
   private final static int nAnglePoints = 10;
   private final static double eccenSamplingExponent = 3.5;

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
      shearDiv = 1.0 / (1.0 - xShear*yShear);
      v2Bound = v1Size + v2Size;
      v3Bound = v2Bound + v3Size;
      hv4Bound = v3Bound + hv4Size;
      v3aBound = v3Bound + v3aSize;

      searchStartAngles = new double[nEccenPoints * nAnglePoints][2];
      double ecc;
      int i, j, k = 0;
      for (i = 0; i < nEccenPoints; ++i) {
         ecc = MAX_ECCENTRICITY * Math.pow(
            ((double)nEccenPoints - i)/((double)nEccenPoints - 1),
            eccenSamplingExponent);
         for (j = 0; j < nAnglePoints; ++j) {
            searchStartAngles[k][0] = 180.0 * j / (double)(nAnglePoints - 1);
            searchStartAngles[k][1] = ecc;
            ++k;
         }
      }
      searchStartPoints = angleToCortex(searchStartAngles);
   }

   // Constants:
   public final static double DOUBLE_SECH_PARAM1 = 0.1821;
   public final static double DOUBLE_SECH_PARAM2 = 0.76;

   // convert to/from standard mathematica coordinates to the theta/rho coordinates of visual angle
   private final double visualToMath(double angle) {
      return Math.PI/180.0 * (90.0 - angle);
   }
   private final double mathToVisual(double angle) {
      return 90.0 - 180.0/Math.PI * angle;
   }
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
   public final static double arcsech(double z) {
      double invz = 1.0 / z;
      return Math.log(invz + Math.sqrt((invz - 1.0)*(1.0 + invz)));
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
   // invert the above transform
   private final void finalRx(double[] z) {
      double x, y;
      // uncenter then unscale...
      x = (z[0] - xFC)/xScale;
      y = (z[1] - yFC)/yScale;
      // unrotate
      z[0] =  x*cosPsi + y*sinPsi;
      z[1] = -x*sinPsi + y*cosPsi;
      // unshear
      x = shearDiv * (z[0] - z[1]*xShear);
      y = shearDiv * (z[1] - z[0]*yShear);
      // unflip and uncenter
      z[0] = x + FC0;
      z[1] = -y;
   }

   // Perform the lambda-transform part of the Schira Model
   private final void lambdaTx(double[] xy) {
      if (xy[0] >= 0) xy[0] += lambda;
      else xy[0] += 2.0 * lambda * (1.0 - Math.abs(Math.atan2(xy[1], xy[0]))/Math.PI);
   }
   private final void lambdaRx(double[] xy) {
      if (xy[0] >= lambda) xy[0] -= lambda;
      else {
         // this is tricky because we have to search for the zero of the equation; the inverse has
         // no closed form that I could find...
         // We find the zero of:      f(x0)      = x1 - x0 - 2 lambda (1 - atan2(y0, x0)/pi)
         // which has the derivative: df(x0)/dx0 = -1 - 2 y0 lambda / (pi (x0^2 + y0^2))
         final double tol = 1e-10; // this is plenty precise for our purposes
         double rz = xy[0];
         double x0 = 0.5 * (2 * rz - lambda);
         double y0 = Math.abs(xy[1]);
         double y02 = y0*y0;
         double f, df;
         double k = 2*lambda/Math.PI;
         for (int i = 0; i < 100; ++i) {
            f  = rz - x0 - k*(Math.PI - Math.atan2(y0, x0));
            df = -1.0 - k*y0 / (x0*x0 + y02);
            if (Math.abs(f) < tol) break;
            x0 -= f / df;
         }
         // The above loop should converge pretty quickly (around 15 iterations?); at the end we
         // have a good estimate of x0.
         xy[0] = x0;
      }
   }
   public final double[] lambdaFwd(double[] xy) {
      double[] r = new double[2];
      r[0] = xy[0]; r[1] = xy[1];
      lambdaTx(r);
      return r;
   }
   public final double[] lambdaInv(double[] xy) {
      double[] r = new double[2];
      r[0] = xy[0]; r[1] = xy[1];
      lambdaRx(r);
      return r;
   }
   // This function performs the layerless part of the Schira Model forward transform in place;
   // it converts {theta, rho} into {x, y}.
   private final void layerlessTx(double[] xy) {
      double absz = Math.sqrt(xy[0]*xy[0] + xy[1]*xy[1]);
      if (Num.zeroish(absz)) {
         xy[0] = Math.log(A/B);
         xy[1] = 0.0;
      } else {
         double argz = Math.atan2(xy[1], xy[0]);
         double numth, divth, numr, numi, divr, divi;
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
         xy[0] = Math.log(numr / divr);
         xy[1] = numth - divth;
      }
   }
   private final double dist2(double[] a, double[] b) {
      double dx = a[0] - b[0];
      double dy = a[1] - b[1];
      return dx*dx + dy*dy;
   }
   private final void layerlessRx(double[] xy) {
      double absz, argz, rz, iz;
      if (Num.zeroish(xy[1]) && xy[0] == Math.log(A/B)) {
         xy[0] = 0;
         xy[1] = 0;
      } else {
         double[] z0 = new double[2];
         double[] z = new double[2];
         double[] dz = new double[2];
         final double tol = 1e-14;
         double delt = 0.1;
         double d, d2, tmp1, tmp2 = -1.0;
         z0[0] = 0.11; z0[1] = -0.27;
         int i;
         // find dist...
         z[0] = z0[0]; z[1] = z0[1]; layerlessTx(z);
         d2 = dist2(xy, z);
         d = Math.sqrt(d2);
         for (i = 0; i < 200 && d2 >= tol; ++i) {
            delt = 0.01 * d;
            // calculate dx:
            z[0] = z0[0] + delt; z[1] = z0[1]; layerlessTx(z);
            dz[0] = dist2(xy, z);
            z[0] = z0[0] - delt; z[1] = z0[1]; layerlessTx(z);
            dz[0] = (dz[0] - dist2(xy, z)) / (2*delt);
            // calculate dy:
            z[0] = z0[0]; z[1] = z0[1] + delt; layerlessTx(z);
            dz[1] = dist2(xy, z);
            z[0] = z0[0]; z[1] = z0[1] - delt; layerlessTx(z);
            dz[1] = (dz[1] - dist2(xy, z)) / (2*delt);
            // step along grad
            tmp1 = d2 / (dz[0]*dz[0] + dz[1]*dz[1]);
            do {
               if (tmp1 < tol) {
                  if (tmp2 < 0)
                     throw new IllegalStateException("stepsize decreased to 0...");
                  else
                     break;
               }
               z[0] = z0[0] - tmp1*dz[0];
               z[1] = z0[1] - tmp1*dz[1];
               layerlessTx(z);
               tmp2 = dist2(xy, z);
               tmp1 *= 0.5;
            } while (tmp2 >= d2);
            d2 = tmp2;
            d = Math.sqrt(d2);
            z0[0] = z0[0] - tmp1*dz[0];
            z0[1] = z0[1] - tmp1*dz[1];
         }
         xy[0] = z0[0];
         xy[1] = z0[1];
      }
   }
   public final double[] layerlessFwd(double[] xy) {
      double[] r = new double[2];
      r[0] = xy[0]; r[1] = xy[1];
      layerlessTx(r);
      return r;
   }
   public final double[] layerlessInv(double[] xy) {
      double[] r = new double[2];
      r[0] = xy[0]; r[1] = xy[1];
      layerlessRx(r);
      return r;
   }
   // convert between coordinate types
   private final void polToCartTx(double[] tr) {
      double arg = tr[0];
      double abs = tr[1];
      tr[0] = abs * Math.cos(arg);
      tr[1] = abs * Math.sin(arg);
   }
   private final void cartToPolTx(double[] xy) {
      double x = xy[0];
      double y = xy[1];
      xy[0] = Math.atan2(y, x);
      xy[1] = Math.sqrt(x*x + y*y);
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
      double sgn = (Num.zeroish(zz)? 1.0 : Math.signum(zz));
      rho = Math.abs(rho);
      // layered part...
      double[] layers = new double[5];
      // set the thetas on which we will operate with the private layerlessTx and finalTx functions
      xy[0][0] = v1Size * zz;
      xy[1][0] = sgn * (v1Size + v2Size*(1.0 - abszz));
      xy[2][0] = sgn * (v1Size + v2Size + v3Size*abszz);
      xy[3][0] = v1Size + v2Size + v3Size + hv4Size*(1.0 - 0.5*(zz + 1.0));
      xy[4][0] = -(v1Size + v2Size + v3Size + 0.5*v3aSize*(zz + 1.0));
      // log-polar part...
      for (int i = 0; i < 5; ++i) {
         xy[i][1] = rho;
         polToCartTx(xy[i]);
         lambdaTx(xy[i]);
         layerlessTx(xy[i]);
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
    *  3.0, 4.0, or -4.0, indicating that (x,y) is in V1, V2, V3, V3A, or hV4, respectively (note
    *  that hV4 and V3A are not predictive --- they are pseudo-areas that stabilize registrations
    *  and should not be considered valid predictions). Additionally, the area-boundaries between
    *  adjacent areas is indicated with 1.5 (V1-V2 border), 2.5 (V2-V3 border), and 3.5 (V3-outer
    *  border); the area value 0 is returned for points outside the outer boundaries of V3A and hV4.
    */                      
   public final double[] cortexToAngle(double x, double y) {
      double[] xy = new double[2];
      xy[0] = x; xy[1] = y;
      // inverse the transforms...
      finalRx(xy);
      layerlessRx(xy);
      lambdaRx(xy);
      cartToPolTx(xy);
      // we now have a theta/rho value that we need to convert back to a visual area
      double[] tra = new double[3];
      double zz = xy[0];
      double abszz = Math.abs(zz);
      double sgn = (Num.zeroish(zz)? 1.0 : Math.signum(zz));
      if (Num.zeroish(abszz - v1Size)) {
         // V1-V2 Border
         tra[0] = sgn;
         tra[2] = 1.5;
      } else if (abszz < v1Size) {
         // Inside V1
         tra[0] = zz/v1Size;
         tra[2] = 1.0;
      } else if (Num.zeroish(abszz - v2Bound)) {
         // V2/V3 boundary
         tra[0] = 0;
         tra[2] = 2.5;
      } else if (abszz < v2Bound) {
         // Inside V2
         tra[0] = sgn*(1.0 - (abszz - v1Size)/v2Size);
         tra[2] = 2.0;
      } else if (Num.zeroish(abszz - v3Bound)) {
         // V3/V4 border
         tra[0] = sgn;
         tra[2] = 3.5;
      } else if (abszz < v3Bound) {
         // Inside V3
         tra[0] = sgn*(abszz - v2Bound)/v3Size;
         tra[2] = 3.0;
      } else if (sgn < 0 && abszz <= v3aBound) {
         // Inside V3A-like area
         tra[0] = 2*(abszz - v3Bound)/v3aSize - 1.0;
         tra[2] = 4.0;
      } else if (sgn > 0 && abszz <= hv4Bound) {
         // Inside hV4-like area
         tra[0] = 2*(1.0 - (abszz - v3Bound)/hv4Size) - 1.0;
         tra[2] = -4.0;
      } else if (sgn < 0) {
         // Outside, dorsal
         tra[0] = (abszz - v3aBound) / (v3aBound - 2.0);
         tra[2] = 0;
      } else {
         // Outside, ventral
         tra[0] = (abszz - hv4Bound) / (2.0 - hv4Bound);
         tra[2] = 0;
      }
      tra[0] = mathToVisual(0.5 * Math.PI * tra[0]);
      tra[1] = xy[1];
      return tra;
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
