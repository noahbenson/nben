////////////////////////////////////////////////////////////////////////////////////////////////////
// Util.java
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

import nben.util.Par;
import nben.util.Num;

/** Util is a clas of static methods that provide operations on spherical geometry objects.
 *
 *  @author Noah C. Benson
 */
public class Util {
   /** Yields the area of the given point (which is always 0) */
   public final double area(Point p) {return 0;}
   /** Yields the area of the given great-circle (which is always 0) */
   public final double area(GreatCircle a) {return 0;}
   /** Yields the area of the given triangle */
   public final double area(Triangle a) {return a.area();}

   /** Yields one of the two points of intersection of the great circles; the other point is
    *  the antipodal point of the given point.
    */
   public static final Point intersection(GreatCircle a, GreatCircle b) {
      return Point._from(
         Num.cross(
            Num.cross(a.A.coords, a.B.coords),
            Num.cross(b.A.coords, b.B.coords)));
   }
}