////////////////////////////////////////////////////////////////////////////////////////////////////
// LineSegment.java
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

/** A LineSegment object stores information about a segment on a greater circle.
 *
 *  @author Noah C. Benson
 */
public class LineSegment {
   /** The first of two points on the arc. */
   public final Point A;
   /** The second of two points on the arc. */
   public final Point B;

   /** s.norm() yields the length of the line segment s. */
   public final double norm() {return Num.euclideanDistance(A.coords, B.coords);}
   /** s.norm2() yields the square of the length of the line segment s. */
   public final double norm2() {return Num.euclideanDistance2(A.coords, B.coords);}
   /** a.line() yields the line object that contains the line segment a. */
   public final Line line() {
      return Line._from(A, B);
   }
   /** a.midpoint() yields the midpoint of the line segment a. */
   public final Point midpoint() {
      return Point._from(0.5 * (A.coords[0] + B.coords[0]),
                         0.5 * (A.coords[1] + B.coords[1]));
   }
   /** a.contains(p) yields true if the arc a contains the point p, otherwise false. */
   public final boolean contains(Point p) {
      if (!line().contains(p)) return false;
      double d = norm2();
      return Num.nonnegative(d - Num.euclideanDistance2(p.coords, A.coords))
         &&  Num.nonnegative(d - Num.euclideanDistance2(p.coords, B.coords));
   }
   /** a.contains(b) yields true if the line segment a contains the line segment b, otherwise
    *  false.
    */
   public final boolean contains(LineSegment b) {
      return contains(b.A) && contains(b.B);
   }

   /** a.intersectionPoint(b) yields the point at which line segments a and b intersect. Note that
    *  if they intersect at a line segment or they do not intersect, then this will yield null. To
    *  test for an line segment intersection, use inersectionLineSegment or the R2.lib.intersection
    *  function.
    */
   public final Point intersectionPoint(LineSegment b) {
      // we find the two intersection points of the line...
      Point p = line().intersectionPoint(b);
      // either p is on this arc, or it isn't; if it's not, we don't intersect
      if (p != null && contains(p)) return p;
      else return null;
   }
   /** a.intersectionSegment(b) yields the line segment at which line segments a and b
    *  intersect, or null if they do not intersect at a line segment.
    */
   public final LineSegment intersectionSegment(LineSegment b) {
      if (!line().equals(b.line())) return null;
      boolean
         aCbA = contains(b.A),
         aCbB = contains(b.B),
         bCaA = b.contains(A),
         bCaB = b.contains(B);
      if      (aCbA && aCbB) return b;
      else if (bCaA && bCaB) return this;
      else if (aCbA && bCaA) return LineSegment.from(b.A, A);
      else if (aCbB && bCaA) return LineSegment.from(b.B, A);
      else if (aCbA && bCaB) return LineSegment.from(b.A, B);
      else                   return null;
   }

   /** s.nearest(p) yields the point q that is the closest point on the line segment g to the point
    *  p; if p is on the line segment g itself, then p is returned.
    */
   public final Point nearest(Point q) {
      Point isect = line().nearest(q);
      if (contains(isect)) return isect;
      else {
         double
            da = Num.euclideanDistance2(A.coords, isect.coords),
            db = Num.euclideanDistance2(B.coords, isect.coords);
         return (da < db? A : B);
      }
   }

   /** Construct an LineSegment object; this should generally be done via the static function 
    *  LineSegment.from().
    */
   protected LineSegment(Point a, Point b) {
      A = a;
      B = b;
   }
   /** LineSegment.from(a, b) yields a line segment between a to b. */
   public static final LineSegment from(Point a, Point b) {
      return new LineSegment(a, b);
   }
   /** LineSegment.from(a, b) yields a line segment between a to b. */
   public static final LineSegment from(double[] a, double[] b) {
      return new LineSegment(Point.from(a), Point.from(b));
   }
   /** LineSegment._from(a, b) yields a line segment between a to b; this is identical to 
    *  LineSegment.from(a,b) except that it does not copy the arrays and does no error checking.
    */
   public static final LineSegment _from(double[] a, double[] b) {
      return new LineSegment(Point._from(a), Point._from(b));
   }

}
