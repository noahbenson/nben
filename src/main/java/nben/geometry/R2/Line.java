////////////////////////////////////////////////////////////////////////////////////////////////////
// Line.java
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

/** A Line object stores information about a line in the plane.
 *
 *  @author Noah C. Benson
 */
public class Line {
   /** The first of two points on the line. */
   public final Point A;
   /** The second of two points on the line. */
   public final Point B;

   /** Yields true if the two lines are parallel and false otherwise. */
   public final boolean parallel(Line l) {
      double[]
         q1 = A.coords,   q2 = B.coords,
         q3 = l.A.coords, q4 = l.B.coords;
      return Num.zeroish((q1[0] - q2[0])*(q3[1] - q4[1]) - (q1[1] - q2[1])*(q3[0] - q4[0]));
   }
   public final boolean parallel(LineSegment s) {return parallel(s.line());}
   
   /** g.contains(p) yields true if the line g contaings the point p. */
   public final boolean contains(Point p) {
      double
         dAP = Num.euclideanDistance(A.coords, p.coords),
         dBP = Num.euclideanDistance(B.coords, p.coords);
      double d = dAP + dBP;
      return Num.eq(d*d, Num.euclideanDistance2(A.coords, B.coords));
   }
   /** g.contains(a) yields true if the line g contains the line segment a. */
   public final boolean contains(LineSegment s) {
      return contains(s.A) && contains(s.B);
   }

   /** g.intersectionPoint(l) yields the intersection point of lines g and c or null if they
    *  do not intersect at a point (ie, they are parallel).
    */
   public final Point intersectionPoint(Line l) {
      double[]
         q1 = A.coords,   q2 = B.coords,
         q3 = l.A.coords, q4 = l.B.coords;
      double
         det12 = Num.det(q1, q2),
         det34 = Num.det(q3, q4),
         xnum = det12*(q3[0] - q4[0]) - det34*(q1[0] - q2[0]),
         ynum = det12*(q3[1] - q4[1]) - det34*(q1[1] - q2[1]),
         den = (q1[0] - q2[0])*(q3[1] - q4[1]) - (q1[1] - q2[1])*(q3[0] - q4[0]);
      if (Num.zeroish(den)) return null;
      else                  return Point._from(xnum/den, ynum/den);
   }
   /** g.intersectionPoint(a) yields the intersection point of line g and line segment a or null if
    *  they do not intersect at a point. Note that if the segment lies on the line or overlaps it,
    *  then null is yielded (check instead g.contains(a)).
    */
   public final Point intersectionPoint(LineSegment s) {
      Point p = intersectionPoint(s.line());
      if (p == null) return null;
      else           return (s.contains(p)? p : null);
   }

   /** Constructs a Line object from a coordinate matrix and indices of the two points;
    *  this is private, as the static from() function should be used to construct a line
    *  object. This constructor performs no error checking.
    */
   protected Line(Point a, Point b) {
      A = a;
      B = b;
   }
   /** Line._from(a, b) is identical to Line.from(a,b) except that it does not perform any error 
    *  checking.
    */
   public static final Line _from(Point u, Point v) {
      return new Line(u, v);
   }
   /** Line.from(a, b) constructs a line object from the Points a and b. */
   public static final Line from(Point u, Point v) {
      if (Num.eq(u.coords, v.coords))
         throw new IllegalArgumentException("Cannot provide two identical points");
      else
         return new Line(u, v);
   }
   /** Line._from(u, v) is identical to Line.from(u,v) except that it performs no error checking */
   public static final Line _from(double[] u, double[] v) {
      return new Line(Point._from(u), Point._from(v));
   }
   /** Line.from(u, v) yields the a Line object representing the line that passes through the 2D
    *  points u and v.
    */
   public static final Line from(double[] u, double[] v) {
      if (u.length != 2 || v.length != 2)
         throw new IllegalArgumentException("Coordinate points must be 2D vectors!");
      if (u == v || Num.eq(u, v))
         throw new IllegalArgumentException("Cannot provide two identical points");
      return Line._from(u.clone(), v.clone());
   }
   /** Line._from(coords, a, b) is identical to Line.from(coords, a, b) except that it performs no 
    *  error checking.
    */
   public static final Line _from(double[][] coords, int a, int b) {
      return new Line(Point._from(coords[a]), Point._from(coords[b]));
   }   
   /** Line.from(coords, a, b) yields the a Line object representing the line that passes through
    *  the points coords[a] and coords[b].
    */
   public static final Line from(double[][] coords, int a, int b) {
      if (coords == null)
         throw new IllegalArgumentException("coordinates must be a n x 2 matrix");
      if (a < 0 || b < 0 || a >= coords.length || b >= coords.length)
         throw new IllegalArgumentException("Coordinate indices out of range!");
      return Line.from(coords[a], coords[b]);
   }
   /** Line._fromT(coords, a, b) is identical to Line.fromT(coords, a, b) except that
    *  it performs no error checking.
    */
   public static final Line _fromT(double[][] coords, int a, int b) {
      double[] x0 = new double[2];
      double[] x1 = new double[2];
      x0[0] = coords[0][a]; x0[1] = coords[1][a];
      x1[0] = coords[0][b]; x1[1] = coords[1][b];
      return Line._from(x0, x1);
   }   
   /** Line.fromT(coords, a, b) is identical to Line.from(coords, a, b) except that it
    *  expects the coords matrix to be a (2 x n) matrix instead of an (n x 2) matrix.
    */
   public static final Line fromT(double[][] coords, int a, int b) {
      if (coords == null || coords.length != 3)
         throw new IllegalArgumentException("coordinates must be a 2 x n matrix");
      int mx = (a > b? a : b);
      if (a < 0 || b < 0 ||
          coords[0].length <= mx || coords[1].length <= mx)
         throw new IllegalArgumentException("insufficient columns in matrix");
      double[] x0 = new double[2];
      double[] x1 = new double[2];
      x0[0] = coords[0][a]; x0[1] = coords[1][a];
      x1[0] = coords[0][b]; x1[1] = coords[1][b];
      return Line.from(x0, x1);
   }
   /** Line.from(Q) yields the line formed by the point Q and the origin. */
   public static final Line from(Point Q) {
      return new Line(Q, Point._from(new double[] {0.0, 0.0}));
   }

   /** g.equals(h) yields true if h and g are equivalent great circles. */
   public boolean equals(Object o) {
      if (o instanceof Line) {
         Line l = (Line)o;
         return parallel(l) && contains(l.A);
      } else
         return false;
   }
   public boolean equals(Line l) {return parallel(l);}
}
