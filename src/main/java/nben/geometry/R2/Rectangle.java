////////////////////////////////////////////////////////////////////////////////////////////////////
// Rectangle.java
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

/** A Rectangle object stores information about a rectangle in a plane.
 *
 *  @author Noah C. Benson
 */
public class Rectangle {
   /** The lower-left point in the rectangle. */
   public final Point lowerLeft;
   /** The upper-right point of the rectangle. */
   public final Point upperRight;
   
   /** rect.width() yields the width of the rectangle rect */
   public final double width() {return upperRight.coords[0] - lowerLeft.coords[0];}
   /** rect.height() yields the height of the rectangle rect */
   public final double height() {return upperRight.coords[1] - lowerLeft.coords[1];}

   /** rect.segLower() yields the LineSegment object for bottom of the rectangle rect. */
   public final LineSegment segLower() {
      return LineSegment.from(lowerLeft, Point._from(upperRight.coords[0], lowerLeft.coords[1]));
   }
   /** rect.segUpper() yields the LineSegment object for top of the rectangle rect. */
   public final LineSegment segUpper() {
      return LineSegment.from(Point._from(lowerLeft.coords[0], upperRight.coords[1]), upperRight);
   }
   /** rect.segLeft() yields the LineSegment object for left side of the rectangle rect. */
   public final LineSegment segLeft() {
      return LineSegment.from(lowerLeft, Point._from(lowerLeft.coords[0], upperRight.coords[1]));
   }
   /** rect.segRight() yields the LineSegment object for right side of the rectangle rect. */
   public final LineSegment segRight() {
      return LineSegment.from(Point._from(upperRight.coords[0], lowerLeft.coords[1]), upperRight);
   }

   /** rect.perimeter() yields the perimeter of the rectangle rect. */
   public final double perimeter() {
      return 2*(width() + height());
   }
   /** rect.area() yields the area of the rectangle rect.
    */
   public final double area() {
      return width()*height();
   }

   /** rect.relation_to(q) yields 1 if the point q is inside the rectangle rect, 0 if it is on the 
    *  boundary, and -1 if it is outside of rect.
    *
    *  @param q the point whose relationship relative to the rectangle is to be tested
    *  @return 1 is q is inside the rectangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Point q) {
      if (q.coords[0] < lowerLeft.coords[0]  || q.coords[1] < lowerLeft.coords[1] ||
          q.coords[0] > upperRight.coords[0] || q.coords[1] > upperRight.coords[1])
         return -1;
      if (q.coords[0] == lowerLeft.coords[0]  || q.coords[1] == lowerLeft.coords[1] ||
          q.coords[0] == upperRight.coords[0] || q.coords[1] == upperRight.coords[1])
         return 0;
      else
         return 1;
   }
   /** rect.relation_to(g) yields 0 if the Line g is on the boundary or straddles the rectangle
    *  rect, and -1 if it is completely outside of rect.
    *
    *  @param g the line whose relationship relative to the rectangle is to be tested
    *  @return 0 if g is on the border of the rectangle or intersects it, and -1 if it is outside
    */
   public final int relation_to(Line g) {
      if (lib.intersection(g, segLower()) != null || lib.intersection(g, segUpper()) != null ||
          lib.intersection(g, segLeft()) != null  || lib.intersection(g, segRight()) != null)
         return 0;
      else
         return -1;
   }
   /** rect.relation_to(a) yields 1 if the line segment a is inside the rectangle rect, 0 if it is
    *  on the boundary or straddles the rectangle, and -1 if it is completely outside of rect.
    *
    *  @param a the line segment whose relationship relative to the rectangle is to be tested
    *  @return 1 is a is inside the rectangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(LineSegment a) {
      boolean
         cA = contains(a.A),
         cB = contains(a.B);
      if (cA && cB) {
         if (Num.eq(a.A.coords[0], lowerLeft.coords[0]) ||
             Num.eq(a.A.coords[0], upperRight.coords[0]) ||
             Num.eq(a.A.coords[1], lowerLeft.coords[1]) ||
             Num.eq(a.A.coords[1], upperRight.coords[1]) ||
             Num.eq(a.B.coords[0], lowerLeft.coords[0]) ||
             Num.eq(a.B.coords[0], upperRight.coords[0]) ||
             Num.eq(a.B.coords[1], lowerLeft.coords[1]) ||
             Num.eq(a.B.coords[1], upperRight.coords[1]))
            return 0;
         else
            return 1;
      } else if (cA || cB ||
                 lib.intersection(a, segLower()) != null ||
                 lib.intersection(a, segUpper()) != null ||
                 lib.intersection(a, segLeft()) != null  ||
                 lib.intersection(a, segRight()) != null) {
         return 0;
      } else {
         return -1;
      }
   }
   /** rect.relation_to(t) yields 1 if the triangle t is inside the rectangle rect, 0 if it is on
    *  the boundary or straddles the rectangle, and -1 if it is completely outside of rect. Note
    *  that as long as there is some intersection a 0 or 1 will be returned.
    *
    *  @param t the triangle whose relationship relative to the rectangle rect is to be tested
    *  @return 1 is t is inside the rectangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Triangle t) {
      int
         relA = relation_to(t.A),
         relB = relation_to(t.B),
         relC = relation_to(t.C);
      if (relA == -1 && relB == -1 && relC == -1) {
         if (t.relation_to(segLower()) == 0 ||
             t.relation_to(segUpper()) == 0 ||
             t.relation_to(segLeft())  == 0 ||
             t.relation_to(segRight()) == 0)
            return 0;
         else
            return -1;
      } else if (relA == 1 && relB == 1 && relC == 1) {
         return 1;
      } else
         return 0;
   }
   /** rect.relation_to(t) yields 1 if the rectangle t is inside the rectangle rect, 0 if it is on
    *  the boundary or straddles the rectangle, and -1 if it is completely outside of rect. Note
    *  that as long as there is some intersection a 0 or 1 will be returned.
    *
    *  @param t the rectangle whose relationship relative to the rectangle rect is to be tested
    *  @return 1 is t is inside the rectangle, 0 if it is on the border, and -1 if it is outside
    */
   public final int relation_to(Rectangle t) {
      int
         ll = relation_to(t.lowerLeft),
         ul = relation_to(Point._from(t.lowerLeft.coords[0], t.upperRight.coords[1])),
         ur = relation_to(t.upperRight),
         lr = relation_to(Point._from(t.upperRight.coords[0], t.lowerLeft.coords[1]));
      if (ll == 1 && ul == 1 && ur == 1 && lr == 1) {
         return 1;
      } else if (ll == -1 && ul == -1 && ur == -1 && lr == -1) {
         // either t contains r or they don't intersect at all
         if (t.contains(lowerLeft) || t.contains(upperRight) ||
             t.contains(Point._from(lowerLeft.coords[0], upperRight.coords[1])) ||
             t.contains(Point._from(upperRight.coords[0], lowerLeft.coords[1])))
            return 0;
         else
            return -1;
      } else {
         return 0;
      }
   }

   /** rect.contains(q) yields true if the given rectangle rect contains the given point q; a point
    *  on the boundary of rect is considered inside rect.
    *
    *  @param q a Point object
    *  @return true if q is inside or on the boundary of the rectangle and false otherwise
    */
   public final boolean contains(Point q) {
      return q.coords[0] >= lowerLeft.coords[0] && q.coords[0] <= upperRight.coords[0]
         &&  q.coords[1] >= lowerLeft.coords[1] && q.coords[1] <= upperRight.coords[1];
   }
   /** rect.contains(a) yields true if the given rectangle rect contains the given line segment a.
    *
    *  @param a a LineSegment object
    *  @return true if a is entirely inside or on the boundary of the rectangle and false otherwise
    */
   public final boolean contains(LineSegment a) {return contains(a.A) && contains(a.B);}
   /** rect.contains(t) yields true if the given rectangle rect contains the given rectangle t.
    *
    *  @param t a Rectangle object
    *  @return true if t is entirely inside or on the boundary of the rectangle and false otherwise
    */
   public final boolean contains(Rectangle t) {
      return contains(t.lowerLeft) && contains(t.upperRight);
   }

   /** Constructs a Rectangle instance from the lower left and upper right points of the rectangle.
    */
   protected Rectangle(Point ll, Point ur) {
      lowerLeft = ll;
      upperRight = ur;
   }
   /** Rectangle._from(a, b) is identical to Rectangle.from(a,b) except that it performs no
    *  error checking.
    */
   public static final Rectangle _from(Point a, Point b) {
      return new Rectangle(a, b);
   }
   /** Rectangle.from(ll, ur) constructs a new Rectangle instance from its lower left and upper
    *  right points.
    */
   public static final Rectangle from(Point ll, Point ur) {
      if (ll.coords[0] > ur.coords[0] || ll.coords[1] > ur.coords[1])
         throw new IllegalArgumentException("Corners given are not (lower left, upper right)");
      return new Rectangle(ll, ur);
   }
   /** Rectangle._from(ll, ur) is identical to Rectangle.from(ll, ur) except that it performs no
    *  error checking. If ll and ur are not the lower-left and upper-right corners of a rectangle,
    *  then the behavior is undefined.
    */
   public static final Rectangle _from(double[] ll, double[] ur) {
      return new Rectangle(Point._from(ll), Point._from(ur));
   }
   /** Rectangle.from(ll, ur) yields a new rectangle object with the given lower left and upper
    *  right corners. */
   public static final Rectangle from(double[] ll, double[] ur) {
      return Rectangle.from(Point.from(ll), Point.from(ur));
   }

   /** rect.equals(o) yields true if rect and o are equivalent rectangles, otherwise false. */
   public boolean equals(Rectangle t) {
      if (t == this) return true;
      else return (lowerLeft.equals(t.lowerLeft) && upperRight.equals(t.upperRight));
   }
   public boolean equals(Object o) {
      if (o instanceof Rectangle) return equals((Rectangle)o);
      else return false;
   }
}
