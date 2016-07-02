////////////////////////////////////////////////////////////////////////////////////////////////////
// Point.java
//
// The nben.geometry.R2 namespace contains code used to represent and compute over planar geometry
// entities.
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

package nben.geometry.R2;

import nben.util.Num;

/** A Point object is capable of computing various data regarding points in the plane.
 *
 *  @author Noah C. Benson
 */
public class Point {
   /** the 2D Cartesian coordinates for the point */
   public final double[] coords;

   /** q.norm() yields the 2D Euclidean norm of the point q. */
   public final double norm() {
      return Num.norm(coords);
   }
   /** q.angle() yields the angle that the point q forms with the positive x-axis. */
   public final double angle() {
      return Math.atan2(coords[1], coords[0]);
   }

   /** Constructs a 2D Euclidean point from x and y coordinates. This constructor is protected; the
    *  Point.from and Point._from static methods are intended for constructing point objects. This
    *  constructor does no error checking and does not copy the array.
    */
   protected Point(double[] _coords) {
      coords = _coords;
   }

   /** Point.from(u) yields a 2D Euclidean geometry Point object representing the given coordinates
    *  in u. The array u must be a 2D array of the x and y coordinates.
    */
   public static final Point from(double[] _coords) {
      if (_coords.length != 2) throw new IllegalArgumentException("coordinate length must be 2");
      return new Point(_coords.clone());
   }
   /** Point._from(u) is identical to Point.from(u) except that it does no error checking and does
    *  not copy the array u.
    */
   public static final Point _from(double[] _coords) {
      return new Point(_coords);
   }
   /** Point.from(x,y) is equivalent to Point.from(new double[] {x,y}). */
   public static final Point from(double x, double y) {
      return Point.from(new double[] {x, y});
   }   
   /** Point._from(x,y) is equivalent to Point._from(new double[] {x,y}). */
   public static final Point _from(double x, double y) {
      return Point._from(new double[] {x, y});
   }

   /** p.equal(q) uses the Num.zeroish function to test for identity between points. */
   public boolean equals(Object o) {
      if (o instanceof Point) return Num.eq(((Point)o).coords, coords);
      else                    return false;
   }
   public boolean equals(Point p) {
      return Num.eq(p.coords, coords);
   }
}
