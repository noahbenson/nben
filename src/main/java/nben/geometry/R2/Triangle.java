////////////////////////////////////////////////////////////////////////////////////////////////////
// Triangle.java
//
// The nben.geometry.R2 namespace contains code used to represent and compute over 2D Euclidean
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

package nben.geometry.R2;

import nben.util.Num;
import nben.geometry.R2.Point;
import nben.geometry.R2.Line;
import nben.geometry.R2.LineSegment;
import nben.geometry.R2.lib;

/** A Triangle object stores information about a triangle in a plane.
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
   
   /** tri.lengthAB() yields the length of side AB */
   public final double lengthAB() {return Num._euclideanDistance(A.coords, B.coords);}
   /** tri.lengthBC() yields the length of side BC */
   public final double lengthBC() {return Num._euclideanDistance(B.coords, C.coords);}
   /** tri.lengthCA() yields the length of side CA */
   public final double lengthCA() {return Num._euclideanDistance(C.coords, C.coords);}
   /** tri.cosA() yields the cosine of the inner angle at point A. */
   public final double cosA() {
      return Num._vector_angle_cos(Num.sub(B.coords, A.coords), Num.sub(C.coords, A.coords));
   }
   /** tri.cosB() yields the cosine of the inner angle at point B. */
   public final double cosB() {
      return Num._vector_angle_cos(Num.sub(C.coords, B.coords), Num.sub(A.coords, B.coords));
         }
   /** tri.cosC() yields the cosine of the inner angle at point C. */
   public final double cosC() {
      return Num._vector_angle_cos(Num.cross(A.coords, C.coords), Num.cross(B.coords, C.coords));
   }
   /** tri.angleA() yields the inner angle at point A */
   public final double angleA() {
      return Math.acos(cosA());
   }
   /** tri.angleB() yields the inner angle at point B */
   public final double angleB() {
      return Math.acos(cosB());
   }
   /** tri.angleC() yields the inner angle at point C */
   public final double angleC() {
      return Math.acos(cosC());
   }

   /** tri.centroid() yields the centroid point of the triangle tri. */
   public final Point centroid() {
      return Point._from((A.coords[0] + B.coords[0] + C.coords[0]) / 3.0,
                         (A.coords[1] + B.coords[1] + C.coords[1]) / 3.0);
   }
   
   /** tri.segAB yields the LineSegment object for side AB */
   public final LineSegment segAB() {
      return LineSegment.from(A, B);
   }
   /** tri.segAB yields the LineSegment object for side BC */
   public final LineSegment segBC() {
      return LineSegment.from(B, C);
   }
   /** tri.segAB yields the LineSegment object for side CA */
   public final LineSegment segCA() {
      return LineSegment.from(C, A);
   }

   /** tri.perimeter() yields the perimeter of the triangle. */
   public final double perimeter() {
      return Num.euclideanDistance(A.coords, B.coords)
         +   Num.euclideanDistance(B.coords, C.coords)
         +   Num.euclideanDistance(C.coords, A.coords);
   }
   /** tri.signed_area() yields the signed area of the triangle, which is positive if the triangle
    *  is ordered counter-clockwise and negative otherwise.
    */
   public final double signed_area() {
      double[]
         p0 = A.coords,
         p1 = B.coords,
         p2 = C.coords;
      return 0.5 * (p0[1]*(p2[0] - p1[0]) + p0[0]*(p1[1] - p2[1]) + p1[0]*p2[1] - p1[1]*p2[0]);
   }
   /** tri.area() yields the area of the triangle. */
   public final double area() {
      return Math.abs(signed_area());
   }

   private final static double twoPI = 2*Math.PI;
   /** Triangle.angle(A, B, C) yields the angle between the vectors formed by the points AB
    *  and AC. This is always the counterclockwise angle from AB to AC; if A, B, C are given in 
    *  clockwise order, this will be an angle greater than Pi. The angle is always given as a
    *  number between 0 and Pi.
    *
    *  @param A a Point object
    *  @param B a Point object
    *  @param C a Point object
    *  @return the counter-clockwise angle from AB to AC
    */
   public static final double angle(Point A, Point B, Point C) {
      double[] ab = Num.sub(B.coords, A.coords);
      double[] ac = Num.sub(C.coords, A.coords);
      double th = Math.atan2(ac[1], ac[0]) - Math.atan2(ab[1], ab[0]);
      while (th < 0) th += twoPI;
      while (th > twoPI) th -= twoPI;
      return th;
   }

   /** tri.relation_to(q) yields 1 if the point q is inside the triangle tri, 0 if it is on the 
    *  boundary, and -1 if it is outside of tri.
    *
    *  @param q the point whose relationship relative to the triangle is to be tested
    *  @return 1 is q is inside the triangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Point q) {
      double a = area();
      if (Num.zeroish(a)) {
         if (Num.eq(q.coords, A.coords)) return 0;
         else return -1;
      }
      double a0 = Triangle._from(q, A, B).area();
      if (Num.positive(a0 - a)) return -1;
      double a1 = Triangle._from(q, B, C).area();
      if (Num.positive(a1 + a0 - a)) return -1;
      double a2 = Triangle._from(q, C, A).area();
      if (Num.positive(a2 + a1 + a0 - a)) return -1;
      if (Num.zeroish(a0) || Num.zeroish(a1) || Num.zeroish(a2)) return 0;
      else return 1;
   }
   /** tri.relation_to(g) yields 0 if the Line g is on the boundary or straddles the triangle tri,
    *  and -1 if it is completely outside of tri.
    *
    *  @param g the line whose relationship relative to the triangle is to be tested
    *  @return 0 if g is on the border of the triangle or intersects it, and -1 if it is outside
    */
   public final int relation_to(Line g) {
      // just check if g intersects any of the segments...
      LineSegment ab = segAB(), bc = segBC(), ca = segCA();
      if (g.intersectionPoint(ab) != null 
          || g.intersectionPoint(bc) != null
          || g.intersectionPoint(ca) != null
          || g.contains(ab) || g.contains(bc) || g.contains(ca))
         return 0;
      else 
         return -1;
   }
   /** tri.relation_to(a) yields 1 if the line segment a is inside the triangle tri, 0 if it is on
    *  the boundary or straddles the triangle, and -1 if it is completely outside of tri.
    *
    *  @param a the line segment whose relationship relative to the triangle is to be tested
    *  @return 1 is a is inside the triangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(LineSegment a) {
      // if there is any intersection, the result is 0
      if (a.intersectionPoint(segAB()) != null
          || a.intersectionPoint(segBC()) != null
          || a.intersectionPoint(segCA()) != null)
         return 0;
      // otherwise, a is inside the triangle only if both of a's vertices are inside the triangle
      boolean cntA = contains(a.A);
      boolean cntB = contains(a.B);
      if (cntA && cntB) return 1;
      else if (cntA || cntB) return 0;
      else return -1;
   }
   /** tri.relation_to(t) yields 1 if the triangle t is inside the triangle tri, 0 if it is on the 
    *  boundary or straddles the triangle, and -1 if it is completely outside of tri.
    *
    *  @param t the triangle whose relationship relative to the triangle tri is to be tested
    *  @return 1 is t is inside the triangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Triangle t) {
      // if any arc intersects with any other arc at a point, the result is always 0:
      LineSegment ab0 = segAB(),  bc0 = segBC(), ca0 = segCA(),
         ab = t.segAB(), bc = t.segBC(), ca = t.segCA();
      if (ab0.intersectionPoint(ab) != null
          || ab0.intersectionPoint(bc) != null
          || ab0.intersectionPoint(ca) != null
          || bc0.intersectionPoint(ab) != null
          || bc0.intersectionPoint(bc) != null
          || bc0.intersectionPoint(ca) != null
          || ca0.intersectionPoint(ab) != null
          || ca0.intersectionPoint(bc) != null
          || ca0.intersectionPoint(ca) != null)
         return 0;
      // otherwise, if all the points in t are contained in this triangle, then the result is 1:
      boolean contA = contains(t.A),
              contB = contains(t.B),
              contC = contains(t.C);
      if (contA && contB && contC) return 1;
      // if some but not all of the points are contained, then the result must necessarily be 0 
      // because the intersection of this triangle and t must be either a single arc or a singe
      // point
      else if (contA || contB || contC) return 0;
      // if none of them are inside, then either there is no intersection, or this triangle is
      // entirely in triangle t:
      else {
         if (t.contains(A) && t.contains(B) && t.contains(C)) return 0;
         else return -1;
      }
   }

   /** tri.contains(q) yields true if the given triangle tri contains the given point q; a point on
    *  the boundary of tri is considered inside tri.
    *
    *  @param q a Point object
    *  @return true if q is inside or on the boundary of the triangle and false otherwise
    */
   public final boolean contains(Point q) {return relation_to(q) >= 0;}
   /** tri.contains(a) yields true if the given triangle tri contains the given line segment a.
    *
    *  @param a a LineSegment object
    *  @return true if a is entirely inside or on the boundary of the triangle and false otherwise
    */
   public final boolean contains(LineSegment a) {return relation_to(a) >= 0;}
   /** tri.contains(t) yields true if the given triangle tri contains the given triangle t.
    *
    *  @param t a Triangle object
    *  @return true if t is entirely inside or on the boundary of the triangle and false otherwise
    */
   public final boolean contains(Triangle t) {return relation_to(t) >= 0;}

   /** tri.address(q, x) yields true if the given point q is inside the given triangle tri and false
    *  if it is outside; it writes the 'address' coordinates into the length-2 array x if the point
    *  is inside the triangle; otherwise it does nothing.
    *
    *  @param q the 3D cartesian or 2D longitude-latitude point to address about the triangle
    *  @param x must be a length-2 vector whose contents will be overwritten with the point q's
    *           address about the triangle; if this is null, then it is ignored; note that the
    *           address of a point is identical no matter whether the triangle is clockwise or
    *           counterclockwise
    *  @return true if q is inside tri (when converted to counter-clockwise order) and false
    *          otherwise
    */
   public final boolean address(Point q, double[] x) {
      // we use barycentric coordinates for this
      double a = area();
      if (Num.zeroish(a)) {
         if (!Num.eq(q.coords, A.coords)) return false;
         if (x != null) {
            x[0] = 1.0/3.0;
            x[1] = 1.0/3.0;
         }
         return true;
      }
      double lc = Triangle._from(q, A, B).area();
      if (lc > a) return false;
      double la = Triangle._from(q, B, C).area();
      if (lc + la > a) return false;
      double lb = Triangle._from(q, C, A).area();
      if (la + lb + lc > a) return false;
      if (x != null) {
         x[0] = lb / a;
         x[1] = lc / a;
      }
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

   /** tri.lookup(x,y) looks up the address (x,y) in the given triangle and yields the point to 
    *  which it corresponds. Note that the Point object will be invalid if the given address is not
    *  a proper address (i.e., a[0] and a[1] must both be in the range [0,1]).
    */
   public final Point lookup(double lb, double lc) {
      if (lc < 0.0 || lb < 0.0 || lb + lc > 1.0)
         throw new IllegalArgumentException("Invalid barycentric coordinates given to lookup");
      double la = (1.0 - (lb + lc));
      return Point._from(la*A.coords[0] + lb*B.coords[0] + lc*C.coords[0],
                         la*A.coords[1] + lb*B.coords[1] + lc*C.coords[1]);
   }
   public final Point lookup(double[] addr) {return lookup(addr[0], addr[1]);}

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

   /** tri.equals(o) yields true if tri and o are equivalent triangles, otherwise false. */
   public boolean equals(Triangle t) {
      if (t == this) return true;
      else return ((A.equals(t.A) && B.equals(t.B) && C.equals(t.C)) ||
                   (A.equals(t.B) && B.equals(t.C) && C.equals(t.A)) ||
                   (A.equals(t.C) && B.equals(t.A) && C.equals(t.B)));
   }
   public boolean equals(Object o) {
      if (o instanceof Triangle) return equals((Triangle)o);
      else return false;
   }
}
