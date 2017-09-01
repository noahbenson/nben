////////////////////////////////////////////////////////////////////////////////////////////////////
// HarmonicLogFunction.java
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
import nben.mesh.registration.IDifferentiatedFunction;
import nben.util.Num;

/** HarmonicLogFunction class simply contains the code for calculating potential and derivative
 *  values of harmonic log functions; i.e., when the edge potential is the square of the log of the
 *  relative change in in distance (r/r0), that is a harmonic-log potential function of
 *  relative-delta-distance.
 *
 *  @author Noah C. Benson
 */
public final class HarmonicLogFunction implements IDifferentiatedFunction {
   public final double scale;
   public final double shape;

   public HarmonicLogFunction(double scale, double shape) {
      this.scale = scale;
      this.shape = shape;
   }
   public HarmonicLogFunction(double scale) {
      this.scale = scale;
      this.shape = 2.0;
   }
   public HarmonicLogFunction() {
      this.scale = 1.0;
      this.shape = 2.0;
   }

   /** HarmonicLogFunction.y(x, x0, s, q) yields the harmonic potential y at the value (x / x0)
    *    given the scale s and the shape q. The formula for the potential is:
    *    y = sgn(x/x0) * s/q * |log(x/x0)|^q.
    *
    *  @param x the value at which to calculate the potential
    *  @param x0 the reference or 0-point of the harmonic
    *  @param s the scale of the harmonic potential function
    *  @param q the shape (exponent) of the harmonic potential function
    */
   static public double y(double x, double x0, double s, double q) {
      if (Num.zeroish(x0)) return Double.POSITIVE_INFINITY;
      x /= x0;
      if (!Num.positive(x)) return Double.POSITIVE_INFINITY;
      x = Math.log(x);
      if (q == 2.0) return (s/q)*x*x;
      else          return s/q * Math.pow(Math.abs(x), q);
   }

   /** HarmonicLogFunction.dy(x, x0, s, q) yields the derivative of the harmonic potential y in
    *  terms of the value x given the zero-point x0, the scale s, and the shape q. The formula for
    *  the derivative is dy/dx = sgn(x/x0) s * |x/x0|^(q - 1).
    *
    *  @param x the value at which to calculate the potential
    *  @param s the scale of the harmonic potential function
    *  @param q the shape (exponent) of the harmonic potential function
    */
   static public double dy(double x, double x0, double s, double q) {
      if (Num.zeroish(x0)) return Double.POSITIVE_INFINITY;
      double x_x0 = x / x0;
      if (!Num.positive(x_x0)) return Double.POSITIVE_INFINITY;
      if (q == 2.0)            return s/x * Math.log(x_x0);
      else                     return s/x * Math.pow(Math.abs(Math.log(x_x0)), q - 1.0);
   }

   // The IDifferentiatedFunction's
   public double y(double x, double x0) {return HarmonicLogFunction.y(x, x0, scale, shape);}
   public double dy(double x, double x0) {return HarmonicLogFunction.dy(x, x0, scale, shape);}
}

