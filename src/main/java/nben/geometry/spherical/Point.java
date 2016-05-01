////////////////////////////////////////////////////////////////////////////////////////////////////
// Point.java
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

/** A Point object is capable of computing various data regarding points on spheres.
 *
 *  @author Noah C. Benson
 */
public class Point {
   /** the 3D Cartesian coordinates for the point */
   public final double[] coords;

   /** q.longitude() yields the longitude of the point (where the origin is always (1,0,0) and
    *  the zenith is always (0,0,1).
    */
   public final double longitude() {
      return Math.atan2(coords[1], coords[0]);
   }
   /** q.latitude() yields the latitude of the point (where the origin is always (1,0,0) and
    *  the zenith is always (0,0,1).
    */
   public final double latitude() {
      return Math.asin(coords[2]);
   }
   /** q.azimuth() is an alias for q.longitude() */
   public final double azimuth() {
      return longitude();
   }
   /** q.inclination() yields the inclination angle of the point from the zenith */
   public final double inclination() {
      return Math.acos(coords[2]);
   }

   /** q.antipodal() yields the antipodal point for q. */
   public final Point antipodal() {
      return new Point(new double[] {-coords[0], -coords[1], -coords[2]});
   }

   /** Constructs a spherical geometry point from x, y, and z coordinates. If 0,0,0 is given,
    *  the point is placed at (1,0,0). If only two double values are given, then they are 
    *  assumed to be (longitude, latitude). This constructor is protected; the Point.from
    *  and Point._from static methods are intended for constructing point objects. This
    *  constructor does no error checking and does not copy the array.
    */
   protected Point(double[] _coords) {
      coords = _coords;
   }

   /** Point.from(u) yields a spherical geometry Point object representing the given coordinates
    *  in u. The array u may be a 3D array (Cartesian coordinates, in which case it is projected
    *  onto the surface of the unit sphere) or a 2D array (spherical coordinates, in which case it
    *  is converted to Cartesian coordinates). Note that if the given coordinates are already
    *  normalized to length 1, a new array is not constructed and the Point holds a reference of the
    *  given array u.
    */
   public static final Point from(double[] _coords) {
      double[] coords;
      if (_coords.length == 2) {
         double cosl = Math.cos(_coords[1]);
         coords = new double[3];
         coords[0] = Math.sin(_coords[0]) * cosl;
         coords[1] = Math.cos(_coords[0]) * cosl;
         coords[2] = Math.sin(_coords[1]);
      } else if (_coords.length == 3) {
         double n = Num.norm2(_coords);
         if (Num.eq(n, 1.0)) {
            coords = _coords;
         } else if (Num.zeroish(n)) {
            coords = new double[3];
            coords[0] = 1.0;
            coords[1] = 0;
            coords[2] = 0;
         } else {
            coords = new double[3];
            n = Math.sqrt(n);
            coords[0] = _coords[0] / n;
            coords[1] = _coords[1] / n;
            coords[2] = _coords[2] / n;
         }
      } else {
         throw new IllegalArgumentException("coordinate length must be 3 or 2");
      }
      return new Point(coords);
   }
   /** Point._from(u) yields a spherical geometry point object from the given unit-length 3D point
    *  represented in Cartesian coordinates. Note that this function does no error checking, so
    *  the point must be valid and must be of unit length or behavior is undefined. Additionally, no
    *  copy is made of the given vector u, so it should not be changed as long as the Point object
    *  persists.
    */
   public static final Point _from(double[] _coords) {
      return new Point(_coords);
   }
   /** Point.from(x,y,z) is equivalent to Point.from(new double[] {x,y,z}). */
   public static final Point from(double x, double y, double z) {
      return Point.from(new double[] {x, y, z});
   }   
   /** Point._from(x,y,z) is equivalent to Point._from(new double[] {x,y,z}). */
   public static final Point _from(double x, double y, double z) {
      return Point._from(new double[] {x, y, z});
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