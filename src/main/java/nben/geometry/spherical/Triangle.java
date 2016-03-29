////////////////////////////////////////////////////////////////////////////////////////////////////
// Triangle.java
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
import nben.geometry.spherical.GreatCircle;
import nben.geometry.spherical.Util;

/** A Triangle object stores information about a triangle on the surface of a sphere.
 *
 *  @author Noah C. Benson
 */
public class Triangle {
   /** The first of three vertices of the triangle. */
   public final Point A;
   /** The second of three vertices of the triangle. */
   public final Point B;
   /** The final of three vertices of the triangle. */
   public final Point C;
   
   /** tri.ccw() yields true if tri is an inner-specified triangle and false if it is an outer-
    *  specified triangle. Inner specified triangles are specified in counter-clockwise ordering
    *  and outer-specified triangles are specified in clockwise ordering.
    */
   public final boolean ccw() {
      return Num.positive(
         Num.dot(
            A.coords,
            Num.cross(
               Num.cross(A.coords, B.coords),
               Num.cross(A.coords, C.coords))));
   }
   /** tri.cw() yields true if tri is an outer-specified triangle and false if it is an inner-
    *  specified triangle. Inner specified triangles are specified in counter-clockwise ordering
    *  and outer-specified triangles are specified in clockwise ordering.
    */
   public final boolean cw() {
      return Num.negative(
         Num.dot(
            A.coords,
            Num.cross(
               Num.cross(A.coords, B.coords),
               Num.cross(A.coords, C.coords))));
   }

   /** tri.as_cw() yields a copy of triangle in which the triangle is expressed in clockwise order,
    *  such that it contains the outside of the triangle.
    */
   public final Triangle as_cw() {
      if (cw()) return this;
      else return Triangle._from(A, C, B);
   }
   /** tri.as_ccw() yields a copy of triangle in which the triangle is expressed in
    *  counter-clockwise order, such that it contains the inside of the triangle.
    */
   public final Triangle as_ccw() {
      if (ccw()) return this;
      else return Triangle._from(A, C, B);
   }

   /** tri.cosAB() yields the cos of the (angular) length of side AB */
   public final double cosAB() {return Num._vector_angle_cos(A.coords, B.coords);}
   /** tri.cosBC() yields the cos of the (angular) length of side BC */
   public final double cosBC() {return Num._vector_angle_cos(B.coords, C.coords);}
   /** tri.cosCA() yields the cos of the (angular) length of side CA */
   public final double cosCA() {return Num._vector_angle_cos(C.coords, A.coords);}
   /** tri.lengthAB() yields the length of side AB */
   public final double lengthAB() {return Num._vector_angle(A.coords, B.coords);}
   /** tri.lengthBC() yields the length of side BC */
   public final double lengthBC() {return Num._vector_angle(B.coords, C.coords);}
   /** tri.lengthCA() yields the length of side CA */
   public final double lengthCA() {return Num._vector_angle(C.coords, A.coords);}
   /** tri.cosA() yields the cosine of the inner angle at point A. */
   public final double cosA() {
      return Num._vector_angle_cos(Num.cross(A.coords, B.coords), Num.cross(A.coords, C.coords));
   }
   /** tri.cosB() yields the cosine of the inner angle at point B. */
   public final double cosB() {
      return Num._vector_angle_cos(Num.cross(B.coords, C.coords), Num.cross(B.coords, A.coords));
   }
   /** tri.cosC() yields the cosine of the inner angle at point C. */
   public final double cosC() {
      return Num._vector_angle_cos(Num.cross(C.coords, A.coords), Num.cross(C.coords, B.coords));
   }
   /** tri.angleA() yields the inner angle at point A */
   public final double angleA() {
      return (ccw()? Math.acos(cosA()) : 2.0*Math.PI - Math.acos(cosA()));
   }
   /** tri.angleB() yields the inner angle at point B */
   public final double angleB() {
      return (ccw()? Math.acos(cosB()) : 2.0*Math.PI - Math.acos(cosB()));
   }
   /** tri.angleC() yields the inner angle at point C */
   public final double angleC() {
      return (ccw()? Math.acos(cosC()) : 2.0*Math.PI - Math.acos(cosC()));
   }

   /** tri.area() yields the area of the triangle. */
   public final double area() {return angleA() + angleB() + angleC() - Math.PI;}
   
   /** Triangle.angle(A, B, C) yields the angle between the great circlea formed by the points AB
    *  and AC. This is always the counterclockwise angle from AB to AC; if A, B, C are given in 
    *  clockwise order, this will be the outside angle.
    *
    *  @param A a Point object
    *  @param B a Point object
    *  @param C a Point object
    *  @return the counter-clockwise angle from AB to AC
    */
   public static final double angle(Point A, Point B, Point C) {
      double[] uab = Num.cross(A.coords, B.coords);
      double[] uac = Num.cross(A.coords, C.coords);
      if (Num.dot(A.coords, Num.cross(uab, uac)) > 0)
         return Num._vector_angle(uab, uac);
      else
         return 2*Math.PI - Num._vector_angle(uab, uac);
   }

   /** tri.relation_to(q) yields 1 if the point q is inside the triangle tri, 0 if it is on the 
    *  boundary, and -1 if it is outside of tri. This respects triangle ordering as an indicator
    *  of an inner/outer triangle.
    *
    *  @param q the point whose relationship relative to the triangle is to be tested
    *  @return 1 is q is inside the triangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Point q) {
      // calculate cross products and dot products...
      if (ccw()) {
         int sab = Num.sign(Num.dot(Num.cross(A.coords, B.coords), q.coords));
         int sbc = Num.sign(Num.dot(Num.cross(B.coords, C.coords), q.coords));
         int sca = Num.sign(Num.dot(Num.cross(C.coords, A.coords), q.coords));
         // if one of these is 0, then we might be on the border...
         if (sab == 0)      return (sbc >= 0 && sca >= 0? 0 : -1);
         else if (sbc == 0) return (sab >= 0 && sca >= 0? 0 : -1);
         else if (sca == 0) return (sab >= 0 && sbc >= 0? 0 : -1);
         else               return (sab == 1 && sbc == 1 && sca == 1? 1 : -1);
      } else {
         // here we switch around the ordering and reverse the response signs:
         int sab = Num.sign(Num.dot(Num.cross(A.coords, C.coords), q.coords));
         int sbc = Num.sign(Num.dot(Num.cross(C.coords, B.coords), q.coords));
         int sca = Num.sign(Num.dot(Num.cross(B.coords, A.coords), q.coords));
         if (sab == 0)      return (sbc >= 0 && sca >= 0? 0 : 1);
         else if (sbc == 0) return (sab >= 0 && sca >= 0? 0 : 1);
         else if (sca == 0) return (sab >= 0 && sbc >= 0? 0 : 1);
         else               return (sab == 1 && sbc == 1 && sca == 1? -1 : 1);
      }
   }

   /** tri.contains(q) yields true if the given triangle tri contains the given point q; note that
    *  this method respects clockwise (outer-triangle) versus counter-clockwise (inner-triangle)
    *  vertex orderings, so the following code correctly yields true:
    *  Triangle.from(new double[] {0.0, 0.0, 1.0},
    *                new double[] {0.0, 1.0, 0.0},
    *                new double[] {1.0, 0.0, 0.0}).contains(Point.from(new double[] {0.0,0.0,-1.0}))
    *
    *  @param q a Point object
    *  @return true if q is inside or on the boundary of the triangle and false otherwise
    */
   public final boolean contains(Point q) {return relation_to(q) == 1;}
   public final boolean contains(double[] q) {return contains(Point.from(q));}

   /** tri.address(q, x) yields true if the given point q is inside the given triangle tri and false
    *  if it is outside; it writes the 'address' coordinates into the length-2 array x if the point
    *  is inside the triangle; otherwise it does nothing. Note that if tri is an outer (clockwise)
    *  triangle, it is converted to an inner (counter-clockwise) triangle for this function. Outer
    *  triangles cannot address points inside of them.
    *
    *  @param q the 3D cartesian or 2D longitude-latitude point to address about the triangle
    *  @param x must be a length-2 vector whose contents will be overwritten with the point q's
    *           address about the triangle; if this is null, then it is ignored; note that the
    *           address of a point is identical no matter whether the triangle is clockwise or
    *           counterclockwise
    *  @return true if q is inside tri (when converted to counter-clockwise order) and false
    *          otherwise
    */
   public final boolean address(Point Q, double[] x) {
      // if we are a clockwise triangle, delegate to a counter-clockwise one
      if (cw()) return as_ccw().address(Q, x);
      // if there is no x; we can delegate to the contains method
      if (x == null) return contains(Q);
      // okay, we're a counter-clockwise triangle and we want to get an address of the point Q;
      // we can do this by finding the point R on B->C such that the great circle AR contains Q
      // then storing the fraction of arclen BC that is BR and the fraction of the arclen of
      double bac = Triangle.angle(A, B, C);
      double baq = Triangle.angle(A, B, Q);
      // if baq is larger than bac, q is not in the triangle...
      if (bac < baq) return false;
      // okay, now find the intersection point of AQ and BC
      Point R = Util.intersection(GreatCircle._from(A, Q), GreatCircle._from(B, C));
      // and get the arclength angles...
      double ar = Num._vector_angle(A.coords, R.coords);
      double aq = Num._vector_angle(A.coords, Q.coords);
      // now, if aq is less than ar, we're good!
      if (ar < aq) return false;
      x[0] = baq / bac;
      x[1] = aq / ar;
      return true;
   }
   /** tri.address(q) yields null if q is not inside the (counter-clockwise oriented) triangle tri
    *  and yields the address of q in tri otherwise. If tri is a clockwise triangle, the calculation
    *  is performed on a reversed triangle.
    */
   public final double[] address(Point Q) {
      double[] x = new double[2];
      if (address(Q, x)) return x;
      else return null;
   }

   /** tri.lookup(a) looks up the address a in the given triangle and yields the point to which it
    *  corresponds. Note that the Point object will be invalid if the given address is not a proper
    *  address (i.e., a[0] and a[1] must both be in the range [0,1]).
    */
   public final Point lookup(double[] addr) {
      // if we are a clockwise triangle, delegate to a counter-clockwise one
      if (cw()) return as_ccw().lookup(addr);

      double baq = angleA() * addr[0];
      // now we need the point R on BC such that angle B->A->R is equal to baq
      double[] cx = Num.cross(B.coords, C.coords);
      double[][] rmtx = Num._rotation_matrix_3d(cx, addr[0] * angleA());
      Point R = Point._from(new double[] {
            Num.dot(rmtx[0], B.coords),
            Num.dot(rmtx[1], B.coords),
            Num.dot(rmtx[2], B.coords)});
      // now, we just find the point that is the appropriate distance from A to R
      cx = Num.cross(A.coords, R.coords);
      rmtx = Num._rotation_matrix_3d(cx, addr[1] * Num._vector_angle(A.coords, R.coords));
      // and that point is the result!
      return Point._from(new double[] {
            Num.dot(rmtx[0], A.coords),
            Num.dot(rmtx[1], A.coords),
            Num.dot(rmtx[2], A.coords)});
   }

   /** Constructs a Triangle instance from the three given points. Note that this does no error
    *  checking. The correct way to construct a triangle is generally via Triangle.from().
    */
   protected Triangle(Point a, Point b, Point c) {
      A = a;
      B = b;
      C = c;
   }
   /** Triangle._from(a, b, c) is identical to Triangle.from(a,b,c) except that it performs no
    *  error checking.
    */
   public static final Triangle _from(Point a, Point b, Point c) {
      return new Triangle(a, b, c);
   }
   /** Triangle.from(a, b, c) constructs a new Triangle instance from points a, b, and c. */
   public static final Triangle from(Point a, Point b, Point c) {
      if (Num.eq(Num.dot(a.coords, b.coords), 1.0)
          || Num.eq(Num.dot(b.coords, c.coords), 1.0)
          || Num.eq(Num.dot(c.coords, a.coords), 1.0))
         throw new IllegalArgumentException("Antipodal/identical points given to Triangle.from()");
      return new Triangle(a, b, c);
   }
   /** Triangle._from(a, b, c) is identical to Triangle.from(a,b,c) except that it performs no
    *  error checking and requires that a, b, anc d all be pre-normalized to unit length. If not,
    *  behavior is undefined.
    */
   public static final Triangle _from(double[] a, double[] b, double[] c) {
      return new Triangle(Point._from(a), Point._from(b), Point._from(c));
   }
   /** Triangle.from(a, b, c) yields a new triangle object with the vertices a, b, and c. */
   public static final Triangle from(double[] a, double[] b, double[] c) {
      Point p = Point.from(a);
      Point q = Point.from(b);
      Point r = Point.from(c);
      // make sure no two points are the same and make sure no two are antipodal
      if (Num.eq(Num.dot(p.coords, q.coords), 1.0)
          || Num.eq(Num.dot(q.coords, r.coords), 1.0)
          || Num.eq(Num.dot(r.coords, p.coords), 1.0))
         throw new IllegalArgumentException("Antipodal/identical points given to Triangle.from()");
      return new Triangle(p, q, r);
   }
   /** Triangle._from(coords, a, b, c) is identical to Triangle.from(coords, a, b, c) except that it
    *  performs no error checking.
    */
   public static final Triangle _from(double[][] coords, int a, int b, int c) {
      return Triangle._from(coords[a], coords[b], coords[c]);
   }
   /** Triangle.from(coords, a, b, c) is equivalent to
    *  Triangle.from(coords[a], coords[b], coords[c]). 
    */
   public static final Triangle from(double[][] coords, int a, int b, int c) {
      return Triangle.from(coords[a], coords[b], coords[c]);
   }
   /** Triangle._fromT(coords, a, b, c) is identical to Triangle.fromT(coords, a, b, c) except that
    *  it performs no error checking.
    */
   public static final Triangle _fromT(double[][] coords, int a, int b, int c) {
      int k = (coords.length > 3? 3 : coords.length);
      double[][] d = new double[3][k];
      for (int i = 0; i < k; ++i) {
         d[0][i] = coords[i][a];
         d[1][i] = coords[i][b];
         d[2][i] = coords[i][c];
      }
      return Triangle._from(d[0], d[1], d[2]);
   }
   /** Triangle.fromT(coords, a, b, c) is equivalent to Triangle.from(coords, a, b, c) except that 
    *  it expects the matrix coords to be transposed relative to Triangle.from.
    */
   public static final Triangle fromT(double[][] coords, int a, int b, int c) {
      if (coords.length != 2 && coords.length != 3)
         throw new IllegalArgumentException("Coordinate matrix must have dimensions 2 or 3 by n");
      return Triangle._fromT(coords, a, b, c);
   }
}
