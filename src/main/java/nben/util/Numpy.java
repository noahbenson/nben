////////////////////////////////////////////////////////////////////////////////////////////////////
// Numpy.java
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

import java.nio.ByteBuffer;
import java.util.Arrays;

/** The Numpy class is a static class that contains functions for deserializing arrays passed over
 *  Py4j to Java; the functions all assume a couple things:
 *   (1) all byte arrays are in big endian order
 *   (2) the first 4 bytes are the number of dimensions
 *   (3) the following 4*dims bytes are the sizes of each dimension
 *   (4) following these, comes the data in row-major order
 *
 *  @author Noah C. Benson
 */
public class Numpy {
   /** Numpy.double2FromBytes(b) yields a double[][] array from the given list of bytes b */
   public static final double[][] double2FromBytes(byte[] b) {
      ByteBuffer bb = ByteBuffer.wrap(b);
      int dims = bb.getInt();
      if (dims != 2)
         throw new IllegalArgumentException("Byte array does not begin with dims=2");
      int n = bb.getInt(),
          m = bb.getInt(),
          i, j;
      // okay, now we read that many doubles...
      double[][] mtx = new double[n][m];
      for (i = 0; i < n; ++i)
         for (j = 0; j < m; ++j)
            mtx[i][j] = bb.getDouble();
      return mtx;
   }
   /** Numpy.double1FromBytes(b) yields a double[] array from the given list of bytes b */
   public static final double[] double1FromBytes(byte[] b) {
      ByteBuffer bb = ByteBuffer.wrap(b);
      int dims = bb.getInt();
      if (dims != 1)
         throw new IllegalArgumentException("Byte array does not begin with dims=1");
      int n = bb.getInt(),
          i, j;
      // okay, now we read that many doubles...
      double[] mtx = new double[n];
      for (i = 0; i < n; ++i)
         mtx[i] = bb.getDouble();
      return mtx;
   }
   /** Numpy.int2FromBytes(b) yields an int[][] array from the given list of bytes b */
   public static final int[][] int2FromBytes(byte[] b) {
      ByteBuffer bb = ByteBuffer.wrap(b);
      int dims = bb.getInt();
      if (dims != 2)
         throw new IllegalArgumentException("Byte array does not begin with dims=2");
      int n = bb.getInt(),
          m = bb.getInt(),
          i, j;
      // okay, now we read that many doubles...
      int[][] mtx = new int[n][m];
      for (i = 0; i < n; ++i)
         for (j = 0; j < m; ++j)
            mtx[i][j] = bb.getInt();
      return mtx;
   }
   /** Numpy.int1FromBytes(b) yields an int[] array from the given list of bytes b */
   public static final int[] int1FromBytes(byte[] b) {
      ByteBuffer bb = ByteBuffer.wrap(b);
      int dims = bb.getInt();
      if (dims != 1)
         throw new IllegalArgumentException("Byte array does not begin with dims=1");
      int n = bb.getInt(),
          i, j;
      // okay, now we read that many doubles...
      int[] mtx = new int[n];
      for (i = 0; i < n; ++i)
         mtx[i] = bb.getInt();
      return mtx;
   }
}