////////////////////////////////////////////////////////////////////////////////////////////////////
// lib.java
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

/** The R2.lib class contains common utility functions for 2D planar geometry.
 *
 *  @author Noah C. Benson
 */
public final class lib {
   /** Yields the point of intersection of the two given lines; if the lines are parallel, then
    *  null is returned, unless the lines are equal, in which case the first argument is returned..
    */
   public static final Object intersection(Line a, Line b) {
      double[] q1 = a.A.coords, q2 = b.B.coords,
               q3 = b.A.coords, q4 = b.B.coords;
      double det12 = Num.det(q1, q2),
             det34 = Num.det(q3, q4);
      double
         xnum = det12*(q3[0] - q4[0]) - det34*(q1[0] - q2[0]),
         ynum = det12*(q3[1] - q4[1]) - det34*(q1[1] - q2[1]),
         den = (q1[0] - q2[0])*(q3[1] - q4[1]) - (q1[1] - q2[1])*(q3[0] - q4[0]);
      if (Num.zeroish(den)) return (a.equals(b)? a : null);
      else                  return Point._from(xnum/den, ynum/den);
   }
   /** Yields the point or line segment of intersection of the given line and line segment; if the
    *  lines have nothing in common, then null is returned.
    */
   public static final Object intersection(Line a, LineSegment b) {
      Object isect = lib.intersection(a, b.line());
      if (isect == null)              return null;
      else if (isect instanceof Line) return b;
      else                            return (b.contains((Point)isect)? isect : null);
   }
   public static final Object intersection(LineSegment a, Line b) {return lib.intersection(b, a);}
   /** Yields the point or line segment of intersection of the given two line segments; if the
    *  segments have nothing in common, then null is returned.
    */
   public static final Object intersection(LineSegment a, LineSegment b) {
      Object isect = lib.intersection(a.line(), b);
      if (isect == null)   return null;
      else if (isect == b) {
         boolean
            aCbA = a.contains(b.A),
            aCbB = a.contains(b.B),
            bCaA = b.contains(a.A),
            bCaB = b.contains(a.B);
         if      (aCbA && aCbB) return b;
         else if (bCaA && bCaB) return a;
         else if (aCbA && bCaA) return LineSegment.from(b.A, a.A);
         else if (aCbB && bCaA) return LineSegment.from(b.B, a.A);
         else if (aCbA && bCaB) return LineSegment.from(b.A, a.B);
         else if (aCbA)         return b.A;
         else if (aCbB)         return b.B;
         else if (bCaA)         return a.A;
         else if (bCaB)         return a.B;
         else                   return null;
      } else               return a.contains((Point)isect)? isect : null;
   }
}
