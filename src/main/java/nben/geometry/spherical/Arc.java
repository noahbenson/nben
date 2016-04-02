////////////////////////////////////////////////////////////////////////////////////////////////////
// Arc.java
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

/** An Arc object stores information about a segment on a greater circle.
 *
 *  @author Noah C. Benson
 */
public class Arc {
   /** The first of two points on the arc. */
   public final Point A;
   /** The second of two points on the arc. */
   public final Point B;
   /** True if the arc is counter-clockwise around A x B, false otherwise. */
   public final boolean direction;

   /** a.normal() yields the normal vector, as a Point, to the plane of the arc a. */
   public final Point normal() {
      return Point._from(Num.cross(A.coords, B.coords));
   }
   /** a.arclength() yields the angular arc-length of the arc a. */
   public final double arclength() {
      return (direction? Num._vector_angle(A.coords, B.coords) 
                       : 2.0*Math.PI - Num._vector_angle(A.coords, B.coords));
   }
   /** a.greatCircle() yields the great circle object that contains the arc a. */
   public final GreatCircle greatCircle() {
      return GreatCircle._from(A, B);
   }
   /** a.midpoint() yields the midpoint of the arc a. */
   public final Point midpoint() {
      double mod = (direction? 0.5 : -0.5);
      double[] x = new double[] {
         mod * (A.coords[0] + B.coords[0]),
         mod * (A.coords[1] + B.coords[1]),
         mod * (A.coords[2] + B.coords[2])};
      Num.normalize(x);
      return Point._from(x);
   }
   /** a.inverse() yields the arc that travels from A to B in the opposite direction */
   public final Arc inverse() {
      return new Arc(A, B, !direction);
   }
   /** a.contains(p) yields true if the arc a contains the point p, otherwise false. */
   public final boolean contains(Point p) {
      if (!greatCircle().contains(p))
         return false;
      else if (Num.zeroish(arclength()
                           - Num._vector_angle(A.coords, p.coords) 
                           - Num._vector_angle(p.coords, B.coords)))
         return direction;
      else
         return !direction;
   }
   /** a.contains(b) yields true if the arc a contains the arc b, otherwise false. */
   public final boolean contains(Arc b) {
      if (direction && b.direction)
         return contains(b.A) && contains(b.B);
      else if (direction)
         return false;
      else if (b.direction)
         return contains(b.A) && contains(b.B) && !b.contains(inverse());
      else
         return !b.inverse().contains(inverse());
   }

   /** a.intersectionPoint(b) yields the point at which arcs a and b intersect. Note that if they
    *  intersect at an arc or they do not intersect, then this will yield null. To test for an 
    *  arc intersection, use inersectionArc.
    */
   public final Point intersectionPoint(Arc b) {
      // we find the two intersection points of the great circles...
      Point p = greatCircle().intersectionPoint(b);
      // either p is on this arc, or it isn't; if it's not, we don't intersect
      if (p != null && contains(p)) return p;
      else return null;
   }
   /** a.intersectionArc(b) yields the arc at which arc a and arc b intersect, or null if they do
    *  not intersect at an arc. Note that if the arcs intersect at two separate arcs (e.g., a would
    *  contain the entire arc b, except that b is a clockwise arc), the result is null.
    */
   public final Arc intersectionArc(Arc b) {
      // #todo
      throw new UnsupportedOperationException("intersectionArc is not yet supported");
   }

   /** Construct an Arc object; this should generally be done via the static functions Arc.ccw and
    *  Arc.cw.
    */
   protected Arc(Point a, Point b, boolean _ccw) {
      A = a;
      B = b;
      direction = _ccw;
   }
   /** Arc.ccw(a, b) yields a counter-clockwise arc from a to b. */
   public static final Arc ccw(Point a, Point b) {
      return new Arc(a, b, true);
   }
   /** Arc._ccw(a, b) is identical to Arc.ccw(a, b) */
   public static final Arc _ccw(Point a, Point b) {
      return new Arc(a, b, true);
   }
   /** Arc.ccw(a, b) yields a counter-clockwise arc from a to b. */
   public static final Arc ccw(double[] a, double[] b) {
      return Arc._ccw(Point.from(a), Point.from(b));
   }
   /** Arc._ccw(a, b) is identical to Arc.ccw(a, b) except that it performs no error checking and
    *  requires that the vectors be pre-normalized to unit length.
    */
   public static final Arc _ccw(double[] a, double[] b) {
      return new Arc(Point._from(a), Point._from(b), true);
   }

   /** Arc.cw(a, b) yields a clockwise arc from a to b. */
   public static final Arc cw(Point a, Point b) {
      return new Arc(a, b, false);
   }
   /** Arc._cw(a, b) is identical to Arc.cw(a, b) */
   public static final Arc _cw(Point a, Point b) {
      return new Arc(a, b, false);
   }
   /** Arc.cw(a, b) yields a clockwise arc from a to b. */
   public static final Arc cw(double[] a, double[] b) {
      return Arc._cw(Point.from(a), Point.from(b));
   }
   /** Arc._cw(a, b) is identical to Arc.cw(a, b) except that it performs no error checking and
    *  requires that the vectors be pre-normalized to unit length.
    */
   public static final Arc _cw(double[] a, double[] b) {
      return new Arc(Point._from(a), Point._from(b), false);
   }
   
}
