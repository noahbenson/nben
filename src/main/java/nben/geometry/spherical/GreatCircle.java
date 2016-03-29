////////////////////////////////////////////////////////////////////////////////////////////////////
// GreatCircle.java
//
// The nben.geometry.spherical namespace contains code used to represent and compute over spherical
// geometry entities.
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

package nben.geometry.spherical;

import nben.util.Num;
import nben.geometry.spherical.Point;

/** A GreatCircle object stores information about a great circle on the surface of a sphere.
 *
 *  @author Noah C. Benson
 */
public class GreatCircle {
   /** The first of two points on the great circle. */
   public final Point A;
   /** The second of two points on the great circle. */
   public final Point B;

   /** Constructs a GreatCircle object from a coordinate matrix and indices of the two points;
    *  this is private, as the static from() function should be used to construct a great
    *  circle object. This constructor performs no error checking.
    */
   protected GreatCircle(Point a, Point b) {
      A = a;
      B = b;
   }
   /** GreatCircle._from(a, b) is identical to GreatCircle.from(a,b) except that it does not
    *  perform any error checking.
    */
   public static final GreatCircle _from(Point u, Point v) {
      return new GreatCircle(u, v);
   }
   /** GreatCircle.from(a, b) constructs a great circle object from the Points a and b. */
   public static final GreatCircle from(Point u, Point v) {
      if (Num.eq(u.coords, v.coords))
         throw new IllegalArgumentException("Cannot provide two identical points");
      else if (Num.zeroish(Num._vector_angle_cos(u.coords, v.coords)))
         throw new IllegalArgumentException("Antipodal points do not specify a great circle!");
      else
         return new GreatCircle(u, v);
   }
   /** GreatCircle._from(u, v) is identical to GreatCircle.from(u,v) except that it performs no
    *  error checking and requires that u and v be pre-normalized; otherwise behavior of the 
    *  great circle object is undefined.
    */
   public static final GreatCircle _from(double[] u, double[] v) {
      return new GreatCircle(Point._from(u), Point._from(v));
   }
   /** GreatCircle.from(u, v) yields the a GreatCircle object representing the great circle
    *  that passes through the projection of the 3D points u and v onto the surface of the unit
    *  sphere.
    */
   public static final GreatCircle from(double[] u, double[] v) {
      if (u.length != 3 || v.length != 3)
         throw new IllegalArgumentException("Coordinate points must be 3D vectors!");
      if (u == v || Num.eq(u, v))
         throw new IllegalArgumentException("Cannot provide two identical points");
      double[] x0 = Num.normalized(u);
      double[] x1 = Num.normalized(v);
      if (Num.eq(Math.abs(Num._vector_angle_cos(x0, x1)), 1.0))
         throw new IllegalArgumentException("Antipodal points do not specify a great circle!");
      return GreatCircle._from(x0, x1);
   }
   /** GreatCircle._from(coords, a, b) is identical to GreatCircle.from(coords, a, b) except that it
    *  performs no error checking and requires that coords[a] and coords[b] be pre-normalized;
    *  otherwise behavior of the great circle object is undefined.
    */
   public static final GreatCircle _from(double[][] coords, int a, int b) {
      return new GreatCircle(Point._from(coords[a]), Point._from(coords[b]));
   }   
   /** GreatCircle.from(coords, a, b) yields the a GreatCircle object representing the great circle
    *  that passes through the projection of the 3D points coords[a] and coords[b] onto the surface
    *  of the unit sphere.
    */
   public static final GreatCircle from(double[][] coords, int a, int b) {
      if (coords == null)
         throw new IllegalArgumentException("coordinates must be a n x 3 matrix");
      if (a < 0 || b < 0 || a >= coords.length || b >= coords.length)
         throw new IllegalArgumentException("Coordinate indices out of range!");
      return GreatCircle.from(coords[a], coords[b]);
   }
   /** GreatCircle._from(coords, a, b) is identical to GreatCircle._from(coords, a, b) except that
    *  it performs no error checking and requires that coords[*][a] and coords[*][b] be
    *  pre-normalized; otherwise behavior of the great circle object is undefined.
    */
   public static final GreatCircle _fromT(double[][] coords, int a, int b) {
      double[] x0 = new double[3];
      double[] x1 = new double[3];
      x0[0] = coords[0][a]; x0[1] = coords[1][a]; x0[2] = coords[2][a];
      x1[0] = coords[0][b]; x1[1] = coords[1][b]; x1[2] = coords[2][b];
      return GreatCircle._from(x0, x1);
   }   
   /** GreatCircle.fromT(coords, a, b) is identical to GreatCircle.from(coords, a, b) except that it
    *  expects the coords matrix to be a 3xn matrix instead of an nx3 matrix.
    */
   public static final GreatCircle fromT(double[][] coords, int a, int b) {
      if (coords == null || coords.length != 3)
         throw new IllegalArgumentException("coordinates must be a 3 x n matrix");
      int mx = (a > b? a : b);
      if (a < 0 || b < 0 ||
          coords[0].length <= mx || coords[1].length <= mx || coords[2].length <= mx)
         throw new IllegalArgumentException("insufficient columns in matrix");
      double[] x0 = new double[3];
      double[] x1 = new double[3];
      x0[0] = coords[0][a]; x0[1] = coords[1][a]; x0[2] = coords[2][a];
      x1[0] = coords[0][b]; x1[1] = coords[1][b]; x1[2] = coords[2][b];
      return GreatCircle.from(x0, x1);
   }
   /** GreatCircle.from(Q) yields the great circle that would form the equator if Q were the zenith
    *  of the sphere.
    */
   public static final GreatCircle from(Point Q) {
      Point R = Point.from(new double[] {Q.coords[0] + 1.0, Q.coords[1], Q.coords[2]});
      Point S = Point._from(Num.cross(R.coords, Q.coords));
      return new GreatCircle(R, S);
   }
}
