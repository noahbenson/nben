////////////////////////////////////////////////////////////////////////////////////////////////////
// Num.java
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

/** The nben.util.Num class is a static class that provides useful interfaces for certain 
 *  numerical-type operations not available in Java's core Math class. These include checking if
 *  a number is close to 0, performing Math operations on arrays of numbers, and similar types
 *  of operations.
 *
 *  @author Noah C. Benson
 */
public final class Num {

   static final double ZERO_TOL = 1e-30;
   static private final double ZERO_TOL_2 = ZERO_TOL*ZERO_TOL;

   /** Num.zeroish(x) yields true if the absolute value of x is less than or equal to the
    *  value Num.ZERO_TOL (equal to 1e-30).
    *
    *  @param x the number whose closeness to zero should be tested
    *  @returns true if Math.abs(x) is less than Num.ZERO_TOL, otherwise false.
    */
   public static final boolean zeroish(double x) {return x*x < ZERO_TOL_2;}
   /** Num.zeroish(x, tol) yields true if the absolute value of x is less than or equal to the
    *  magnitude of tol. The actual check performed is x*x less than tol*tol.
    *
    *  @param x the number whose closeness to zero should be tested
    *  @param tol tolerance cutoff to which x should be compared
    *  @returns true if Math.abs(x) is less than tol, otherwise false.
    */
   public static final boolean zeroish(double x, double tol) {return x*x < tol*tol;}
   /** Num.zeroish(x) yields an array of boolean values the same length as the array x, each
    *  element of which is true if the absolute value of the equivalent value in x is less than or
    *  equal to the value Num.ZERO_TOL (equal to 1e-30).
    *
    *  @param x the array of numbers whose closeness to zero should be tested
    *  @returns a boolean array q, each element q[i] of which is true if Math.abs(x[i]) is less than
    *           Num.ZERO_TOL, otherwise false.
    */
   static final boolean[] zeroish(double[] x) {
      if (x == null) return null;
      boolean[] q = new boolean[x.length];
      for (int i = 0; i < x.length; ++i) q[i] = x[i]*x[i] < ZERO_TOL_2;
      return q;
   }
   /** Num.zeroish(x, tol) yields an array of boolean values the same length as the array x,
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

   /** Num.positive(x) yields true if x is greater than 0; note that if Num.zeroish(x) would
    *  yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @return true if x is positive-ish otherwise false
    */
   public static final boolean positive(double x) {
      return x > ZERO_TOL;
   }
   /** Num.positive(x, tol) yields true if x is greater than 0; note that if 
    *  Num.zeroish(x, tol) would yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return true if x is positive-ish otherwise false
    */
   public static final boolean positive(double x, double tol) {
      return x > Math.abs(tol);
   }
   /** Num.positive(x, tol) yields an array of boolean values, such that result[i] is equal to
    *  Num.positive(x[i], tol).
    *
    *  @param x the array of numbers to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return an array of values that are true if x[i] is positive-ish otherwise false
    */
   public static final boolean[] positive(double[] x, double tol) {
      boolean[] b = new boolean[x.length];
      tol = Math.abs(tol);
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] > tol);
      return b;
   }
   /** Num.positive(x) yields an array of boolean values, such that result[i] is equal to
    *  Num.positive(x[i]).
    *
    *  @param x the array of numbers to test
    *  @return an array of values that are true if x[i] is positive-ish otherwise false
    */
   public static final boolean[] positive(double[] x) {
      boolean[] b = new boolean[x.length];
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] > ZERO_TOL);
      return b;
   }

   /** Num.negative(x) yields true if x is less than 0; note that if Num.zeroish(x) would
    *  yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @return true if x is negative-ish otherwise false
    */
   public static final boolean negative(double x) {
      return x < -ZERO_TOL;
   }
   /** Num.negative(x, tol) yields true if x is less than 0; note that if 
    *  Num.zeroish(x, tol) would yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return true if x is negative-ish otherwise false
    */
   public static final boolean negative(double x, double tol) {
      return x < -Math.abs(tol);
   }
   /** Num.negative(x, tol) yields an array of boolean values, such that result[i] is equal to
    *  Num.negative(x[i], tol).
    *
    *  @param x the array of numbers to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return an array of values that are true if x[i] is negative-ish otherwise false
    */
   public static final boolean[] negative(double[] x, double tol) {
      boolean[] b = new boolean[x.length];
      tol = -Math.abs(tol);
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] < tol);
      return b;
   }
   /** Num.negaive(x) yields an array of boolean values, such that result[i] is equal to
    *  Num.negative(x[i]).
    *
    *  @param x the array of numbers to test
    *  @return an array of values that are true if x[i] is negative-ish otherwise false
    */
   public static final boolean[] negative(double[] x) {
      boolean[] b = new boolean[x.length];
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] < -ZERO_TOL);
      return b;
   }

   /** Num.nonnegative(x) yields true if x is greater than 0; note that if Num.zeroish(x) would
    *  yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @return true if x is nonnegative-ish otherwise false
    */
   public static final boolean nonnegative(double x) {
      return x > -ZERO_TOL;
   }
   /** Num.nonnegative(x, tol) yields true if x is greater than 0; note that if 
    *  Num.zeroish(x, tol) would yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return true if x is nonnegative-ish otherwise false
    */
   public static final boolean nonnegative(double x, double tol) {
      return x > -Math.abs(tol);
   }
   /** Num.nonnegative(x, tol) yields an array of boolean values, such that result[i] is equal to
    *  Num.nonnegative(x[i], tol).
    *
    *  @param x the array of numbers to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return an array of values that are true if x[i] is nonnegative-ish otherwise false
    */
   public static final boolean[] nonnegative(double[] x, double tol) {
      boolean[] b = new boolean[x.length];
      tol = -Math.abs(tol);
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] > tol);
      return b;
   }
   /** Num.nonnegative(x) yields an array of boolean values, such that result[i] is equal to
    *  Num.nonnegative(x[i]).
    *
    *  @param x the array of numbers to test
    *  @return an array of values that are true if x[i] is nonnegative-ish otherwise false
    */
   public static final boolean[] nonnegative(double[] x) {
      boolean[] b = new boolean[x.length];
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] > -ZERO_TOL);
      return b;
   }

   /** Num.nonpositive(x) yields true if x is less than 0; note that if Num.zeroish(x) would
    *  yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @return true if x is nonpositive-ish otherwise false
    */
   public static final boolean nonpositive(double x) {
      return x < ZERO_TOL;
   }
   /** Num.nonpositive(x, tol) yields true if x is less than 0; note that if 
    *  Num.zeroish(x, tol) would yield true, this will always yield false.
    *
    *  @param x the number to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return true if x is nonpositive-ish otherwise false
    */
   public static final boolean nonpositive(double x, double tol) {
      return x < Math.abs(tol);
   }
   /** Num.nonpositive(x, tol) yields an array of boolean values, such that result[i] is equal to
    *  Num.nonpositive(x[i], tol).
    *
    *  @param x the array of numbers to test
    *  @param tol the tolerance cutoff for considering a number equal to 0
    *  @return an array of values that are true if x[i] is nonpositive-ish otherwise false
    */
   public static final boolean[] nonpositive(double[] x, double tol) {
      boolean[] b = new boolean[x.length];
      tol = Math.abs(tol);
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] < tol);
      return b;
   }
   /** Num.nonpositive(x) yields an array of boolean values, such that result[i] is equal to
    *  Num.nonpositive(x[i]).
    *
    *  @param x the array of numbers to test
    *  @return an array of values that are true if x[i] is nonpositive-ish otherwise false
    */
   public static final boolean[] nonpositive(double[] x) {
      boolean[] b = new boolean[x.length];
      for (int i = 0; i < x.length; ++i)
         b[i] = (x[i] < ZERO_TOL);
      return b;
   }

   /** Num.sign(x) yields 1 if x is positive, 0 if x is zeroish (see Num.zeroish), and -1 if 
    *  x is negative.
    *
    *  @param x the number to be tested
    *  @return -1, 0, or 1 if x is negative, zero-ish, or positive
    */
   public static final int sign(double x) {
      if (x > ZERO_TOL) return 1;
      else if (x < -ZERO_TOL) return -1;
      else return 0;
   }
   /** Num.sign(x, tol) yields 1 if x is positive, 0 if x is zeroish (see Num.zeroish), and -1 if 
    *  x is negative. Zero is tested for via Num.zeroish(x, tol).
    *
    *  @param x the number to be tested
    *  @param tol the zero-tolerance cutoff
    *  @return -1, 0, or 1 if x is negative, zero-ish, or positive
    */
   public static final int sign(double x, double tol) {
      tol = Math.abs(tol);
      if (x > tol) return 1;
      else if (x < -tol) return -1;
      else return 0;
   }
   /** Num.sign(x, tol) yields an array y such that y[i] == Num.sign(x[i], tol).
    *
    *  @param x the array of numbers to be tested
    *  @param tol the zero-tolerance cutoff
    *  @return -1, 0, or 1 for each element of x if it is negative, zero-ish, or positive
    */
   public static final int[] sign(double[] x, double tol) {
      if (x == null) return null;
      int[] res = new int[x.length];
      tol = Math.abs(tol);
      for (int i = 0; i < x.length; ++i)
         res[i] = (x[i] > tol? 1 : (x[i] < -tol? -1 : 0));
      return res;
   }
   /** Num.sign(x) yields an array y such that y[i] == Num.sign(x[i]).
    *
    *  @param x the array of numbers to be tested
    *  @return -1, 0, or 1 for each element of x if it is negative, zero-ish, or positive
    */
   public static final int[] sign(double[] x) {
      if (x == null) return null;
      int[] res = new int[x.length];
      for (int i = 0; i < x.length; ++i)
         res[i] = (x[i] > ZERO_TOL? 1 : (x[i] < -ZERO_TOL? -1 : 0));
      return res;
   }

   /** Num.cross(a, b) yields the cross-product vector of the two 3-dimensional bectors a and b.
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

   /** Num._dot(u, v) yields the dot product of vectors u and v; note that this function
    *  performs no error checking.
    *
    *  @param u a n-dimensional vector
    *  @param v a n-dimensional vector
    *  @return the dot product of u . v
    */
   public static final double _dot(double[] u, double[] v) {
      double d = 0;
      for (int i = 0; i < u.length; ++i)
         d += u[i]*v[i];
      return d;
   }
   /** Num.dot(u, v) yields the dot product of vectors u and v.
    *
    *  @param u a n-dimensional vector
    *  @param v a n-dimensional vector
    *  @return the dot product of u . v
    */
   public static final double dot(double[] u, double[] v) {
      if (u.length != v.length)
         throw new IllegalArgumentException("vectors u and v must be the same length!");
      return Num._dot(u, v);
   }

   /** Num.is_matrix(u) yields true if the given object u is a matrix.
    *
    *  @param u a 2D double array
    *  @return true if u is a matrix and false otherwise.
    */
   public static final boolean is_matrix(double[][] u) {
      if (u == null || u.length == 0 || u[0] == null)
         return false;
      int n = u[0].length;
      if (n == 0) return false;
      for (int i = 1; i < n; ++i) {
         if (u[i].length != n) return false;
      }
      return true;
   }
   /** Num.eq(u, v) yields true if either u == v or u == v element-wise. */
   public static final boolean eq(double u, double v) {
      return Num.zeroish(u - v);
   }
   public static final boolean eq(double[] u, double[] v) {
      if (u == v) return true;
      else if (u == null || v == null) return false;
      else if (u.length != v.length) return false;
      else {
         for (int i = 0; i < u.length; ++i) {
            if (!eq(u[i], v[i])) return false;
         }
         return true;
      }
   }
   public static final boolean eq(double[][] u, double[][] v) {
      if (u == v) return true;
      else if (u == null || v == null) return false;
      else if (u.length != v.length) return false;
      else {
         for (int i = 0; i < u.length; ++i) {
            if (!eq(u[i], v[i])) return false;
         }
         return true;
      }
   }
   public static final boolean eq(double[][][] u, double[][][] v) {
      if (u == v) return true;
      else if (u == null || v == null) return false;
      else if (u.length != v.length) return false;
      else {
         for (int i = 0; i < u.length; ++i) {
            if (!eq(u[i], v[i])) return false;
         }
         return true;
      }
   }
   public static final boolean eq(double[][][][] u, double[][][][] v) {
      if (u == v) return true;
      else if (u == null || v == null) return false;
      else if (u.length != v.length) return false;
      else {
         for (int i = 0; i < u.length; ++i) {
            if (!eq(u[i], v[i])) return false;
         }
         return true;
      }
   }

   /** Num.norm2(u) yields the n-dimensional squared euclidean norm of the vector u. */
   public static final double norm2(double[] u) {
      double d2 = 0;
      for (int i = 0; i < u.length; ++i) d2 += u[i]*u[i];
      return d2;
   }
   /** Num.norm(u) yields the n-dimensional euclidean norm of the vector u. */
   public static final double norm(double[] u) {
      return Math.sqrt(norm2(u));
   }
   /** Num.normalize(u) normalizes u in place and yields the norm of u */
   public static final double normalize(double[] u) {
      double nrm = norm(u);
      if (Num.zeroish(nrm)) return 0.0;
      for (int i = 0; i < u.length; ++i)
         u[i] /= nrm;
      return nrm;
   }
   /** Num.normalized(u) yields a new vector in the same direction as u but normalized to have
    *  unit length; if u has a zero length (via Num.zeroish), a 0-length vector is yielded.
    */
   public static final double[] normalized(double[] u) {
      if (u == null) return null;
      double nrm = norm(u);
      double[] v = new double[u.length];
      if (Num.zeroish(nrm)) {
         for (int i = 0; i < u.length; ++i)
            v[i] = 0.0;
      } else {
         for (int i = 0; i < u.length; ++i)
            v[i] = u[i] / nrm;
      }
      return v;
   }

   /** Num._rows_norm2(u) is identical to Num.rows_norm2(u) except that it performs no
    *  error checking.
    */
   public static final double[] _rows_norm2(double[][] u) {
      double[] nrm2s = new double[u.length];
      for (int i = 0; i < u.length; ++i)
         nrm2s[i] = Num.norm2(u[i]);
      return nrm2s;
   }
   /** Num.rows_norm2(u) yields an array of the squared norms of each of the rows of the matrix
    *  u.
    */
   public static final double[] rows_norm2(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _rows_norm2(u);
   }
   /** Num._rows_norm(u) is identical to Num.rows_norm(u) except that it performs no
    *  error checking.
    */
   public static final double[] _rows_norm(double[][] u) {
      double[] nrms = new double[u.length];
      for (int i = 0; i < u.length; ++i)
         nrms[i] = Num.norm(u[i]);
      return nrms;
   }
   /** Num.rows_norm(u) yields an array of the norms of each of the rows of the matrix u. */
   public static final double[] rows_norm(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _rows_norm(u);
   }

   /** Num._cols_norm2(u) is identical to Num.cols_norm2(u) except that it performs no
    *  error checking.
    */
   public static final double[] _cols_norm2(double[][] u) {
      int i, j, m = u[0].length;
      double[] nrm2s = new double[m];
      for (i = 0; i < u.length; ++i) {
         for (j = 0; j < m; ++j)
            nrm2s[j] += u[i][j]*u[i][j];
      }
      return nrm2s;
   }
   /** Num.cols_norm2(u) yields an array of the squared-norms of each of the cols of the matrix
    *   u.
    */
   public static final double[] cols_norm2(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _cols_norm2(u);
   }
   /** Num._cols_norm(u) is identical to Num.cols_norm(u) except that it performs no
    *  error checking.
    */
   public static final double[] _cols_norm(double[][] u) {
      double[] nrms = _cols_norm2(u);
      for (int i = 0; i < u.length; ++i)
         nrms[i] = Math.sqrt(nrms[i]);
      return nrms;
   }
   /** Num.cols_norm(u) yields an array of the norms of each of the cols of the matrix u. */
   public static final double[] cols_norm(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _cols_norm(u);
   }

   /** Num._normalize_rows(u) is identical to Num.normalize_rows(u) except that it performs
    *  no error checking. Note that if a row of u has a length of 1, it is not touched by this
    *  method.
    */
   public static final double[] _normalize_rows(double[][] u) {
      double[] nrms = _rows_norm(u);
      int i,j;
      for (i = 0; i < nrms.length; ++i) {
         if (!Num.zeroish(nrms[i] - 1.0)) {
            for (j = 0; j < u[i].length; ++j)
               u[i][j] /= nrms[i];
         }
      }
      return nrms;
   }
   /** Num.normalize_rows(u) normalizes u in place and yields a list of the norms of the rows
    *  of u.
    */
   public static final double[] normalize_rows(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _normalize_rows(u);
   }
   /** Num._normalize_cols(u) is identical to Num.normalize_cols(u) except that it performs
    *  no error checking. Note that if a col of u has a length of 1, it is not touched by this
    *  method.
    */
   public static final double[] _normalize_cols(double[][] u) {
      double[] nrms = _cols_norm(u);
      int i,j;
      for (j = 0; j < nrms.length; ++j) {
         if (!Num.zeroish(nrms[j] - 1.0)) {
            for (i = 0; i < u.length; ++i)
               u[i][j] /= nrms[j];
         }
      }
      return nrms;
   }
   /** Num.normalize_cols(u) normalizes u in place and yields a list of the norms of the cols
    *  of u.
    */
   public static final double[] normalize_cols(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _normalize_cols(u);
   }

   /** Num._normalized_rows(u) is identical to Num.normalized_rows(u) except that it
    *  performs no error checking.
    */
   public static final double[][] _normalized_rows(double[][] u) {
      double[] nrms = _rows_norm(u);
      double[][] v = new double[u.length][u[0].length];
      int i,j;
      for (i = 0; i < nrms.length; ++i) {
         for (j = 0; j < u[i].length; ++j)
            v[i][j] = u[i][j] / nrms[i];
      }
      return v;
   }
   /** Num.normalized_rows(u) normalizes the rows of u in a new matrix and yields that new
    *  matrix.
    */
   public static final double[][] normalized_rows(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _normalized_rows(u);
   }
   /** Num._normalized_cols(u) is identical to Num.normalized_cols(u) except that it
    *  performs no error checking.
    */
   public static final double[][] _normalized_cols(double[][] u) {
      double[] nrms = _cols_norm(u);
      double[][] v = new double[u.length][u[0].length];
      int i,j;
      for (j = 0; j < nrms.length; ++j) {
         for (i = 0; i < u.length; ++i)
            v[i][j] = u[i][j] / nrms[j];
      }
      return v;
   }
   /** Num.normalized_cols(u) normalizes the columns of u in a new matrix and yields that new
    *  matrix.
    */
   public static final double[][] normalized_cols(double[][] u) {
      if (!Num.is_matrix(u))
         throw new IllegalArgumentException("Given argument u is not a matrix!");
      return _normalized_cols(u);
   }

   /** Num.vector_angle_cos(u, v) yields the cosine of the vector angle between two vectors u
    *  and v.
    */
   public static final double vector_angle_cos(double[] u, double[] v) {
      if (u == null || v == null || u.length != v.length)
         throw new IllegalArgumentException("Arguments must be same-length vectors");
      double du2 = norm2(u);
      double dv2 = norm2(v);
      double udotv = Num._dot(u, v);
      if (du2 == 1.0 && dv2 == 1.0) return udotv;
      else return udotv / Math.sqrt(du2 * dv2);
   }
   /** Num.vector_angle(u, v) yields the vector angle between two vectors u and v. */
   public final double vector_angle(double[] u, double[] v) {
      return Math.acos(vector_angle_cos(u, v));
   }

   /** Num._vector_angle_cos(u, v) yields the cosine of the vector angle between two vectors u
    *  and v; u and v must be normalized and no error checking is performed.
    */
   public static final double _vector_angle_cos(double[] u, double[] v) {
      return Num._dot(u, v);
   }
   /** Num._vector_angle(u, v) yields the vector angle between two vectors u and v; u and v must
    *  be normalized and no error checking is performed
    */
   public static final double _vector_angle(double[] u, double[] v) {
      return Math.acos(Num._dot(u, v));
   }

   /** Num._rotation_matrix_3d(x, theta) is identical to Num.rotation_matrix_3d except that it
    *  requires that the vector x be normalized to unit length; otherwise behavior is undefined.
    */
   public static final double[][] _rotation_matrix_3d(double[] u, double theta) {
      // we used the Euler-Rodrigues formula;
      // see https://en.wikipedia.org/wiki/Euler-Rodrigues_formula
      double a = Math.cos(0.5*theta), s = -Math.sin(0.5*theta);
      double b = s*u[0], c = s*u[1], d = s*u[2];
      double a2 = a*a, b2 = b*b, c2 = c*c, d2 = d*d;
      double bc = b*c, ad = a*d, ac = a*c, ab = a*b, bd = b*d, cd = c*d;
      double[][] res = new double[3][3];

      res[0][0] = a2 + b2 - c2 - d2;
      res[0][1] = 2*(bc + ad);
      res[0][2] = 2*(bd - ac);

      res[1][0] = 2*(bc - ad);
      res[1][1] = a2 + c2 - b2 - d2;
      res[1][2] = 2*(cd + ab);
      
      res[2][0] = 2*(bd + ac);
      res[2][1] = 2*(cd - ab);
      res[2][2] = a2 + d2 - b2 - c2;

      return res;
   }
   /** Num.rotation_matrix_3d(x, theta) yields a matrix that will rotate any given point y around
    *  the point x in the conter-clockwise direction by theta radians.
    */
   public static final double[][] rotation_matrix_3d(double[] u, double theta) {
      return _rotation_matrix_3d(normalized(u), theta);
   }

   /** Num._alignment_matrix_3d(u, v) is identical to Num.alignment_matrix_3d except that it
    *  requires that the vectors u and v be pre-normalized to unit length.
    */
   public static final double[][] _alignment_matrix_3d(double[] u, double[] v) {
      return _rotation_matrix_3d(cross(u, v), _vector_angle(u, v));
   }
   /** Num.alignment_matrix_3d(u, v) yields the rotation matrix that will align the point u to the
    *  point v.
    */
   public static final double[][] alignment_matrix_3d(double[] u, double[] v) {
      return _alignment_matrix_3d(normalized(u), normalized(v));
   }

   /** Num.range(k) yields an array of the numbers 0 to k-1. Num.range(0) yields a 0-length array,
    *  while Num.range(q) for q less than 0 yields null.
    */
   public static final int[] range(int k) {
      if (k < 0)
         return null;
      int[] res = new int[k];
      for (int i = 0; i < k; ++i)
         res[i] = i;
      return res;
   }
   /** Num.range(s, k) yields an array of the numbers s to k-1. Num.range(s, s) yields a 0-length
    *  array while Num.range(s, k) for k less than s yields null.
    */
   public static final int[] range(int s, int k) {
      if (k < s)
         return null;
      int[] res = new int[k - s];
      for (int i = s; i < k; ++i)
         res[i - s] = i;
      return res;
   }
}
