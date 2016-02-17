////////////////////////////////////////////////////////////////////////////////////////////////////
// Numbers.java
//
// The nben.util namespace contains functions that are generally useful across the nben JVM library.
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

package nben.util;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Arrays;

/** The nben.util.Numbers class is a static class that provides useful interfaces for certain 
 *  numerical-type operations not available in Java's core Math class. These include checking if
 *  a number is close to 0, performing Math operations on arrays of numbers, and similar types
 *  of operations.
 *
 *  @author Noah C. Benson
 */
public final class Numbers {

   static final double ZERO_TOL = 1e-30;
   static private final double ZERO_TOL_2 = ZERO_TOL*ZERO_TOL;

   /** Numbers.zeroish(x) yields true if the absolute value of x is less than or equal to the
    *  value Numbers.ZERO_TOL (equal to 1e-30).
    *
    *  @param x the number whose closeness to zero should be tested
    *  @returns true if Math.abs(x) is less than Numbers.ZERO_TOL, otherwise false.
    */
   public static final boolean zeroish(double x) {return x*x < ZERO_TOL_2;}
   /** Numbers.zeroish(x, tol) yields true if the absolute value of x is less than or equal to the
    *  magnitude of tol. The actual check performed is x*x less than tol*tol.
    *
    *  @param x the number whose closeness to zero should be tested
    *  @param tol tolerance cutoff to which x should be compared
    *  @returns true if Math.abs(x) is less than tol, otherwise false.
    */
   public static final boolean zeroish(double x, double tol) {return x*x < tol*tol;}
   /** Numbers.zeroish(x) yields an array of boolean values the same length as the array x, each
    *  element of which is true if the absolute value of the equivalent value in x is less than or
    *  equal to the value Numbers.ZERO_TOL (equal to 1e-30).
    *
    *  @param x the array of numbers whose closeness to zero should be tested
    *  @returns a boolean array q, each element q[i] of which is true if Math.abs(x[i]) is less than
    *           Numbers.ZERO_TOL, otherwise false.
    */
   static final boolean[] zeroish(double[] x) {
      if (x == null) return null;
      boolean[] q = new boolean[x.length];
      for (int i = 0; i < x.length; ++i) q[i] = x[i]*x[i] < ZERO_TOL_2;
      return q;
   }
   /** Numbers.zeroish(x, tol) yields an array of boolean values the same length as the array x,
    *  each element of which is true if the absolute value of the equivalent value in x is less than
    *  or equal to the magnitude of tol; note that the actual check performed is whether x[i]*x[i]
    *  is less than tol*tol.
    *
    *  @param x the array of numbers whose closeness to zero should be tested
    *  @param tol tolerance cutoff to which each element of x should be compared
    *  @returns a boolean array q, each element q[i] of which is true if Math.abs(x[i]) is less than
    *           tol, otherwise false.
    */
   public static final boolean[] zeroish(double[] x, double tol) {
      if (x == null) return null;
      tol *= tol;
      boolean[] q = new boolean[x.length];
      for (int i = 0; i < x.length; ++i) q[i] = x[i]*x[i] < tol;
      return q;
   }

   /** Numbers.cross(a, b) yields the cross-product vector of the two 3-dimensional bectors a and b.
    *
    *  @param a a 3D vector
    *  @param b a 3D vector
    *  @return the cross product of a and b: a x b.
    */
   public static final double[] cross(double[] a, double[] b) {
      double[] c = new double[3];
      c[0] = a[1]*b[2] - a[2]*b[1];
      c[1] = a[2]*b[0] - a[0]*b[2];
      c[2] = a[0]*b[1] - a[1]*b[0];
      return c;
   }

}
