////////////////////////////////////////////////////////////////////////////////////////////////////
// Fields.java
//
// The nben.mesh.registration namespace contains functions related to the registration of 
// surface meshes to models defined on the cortical surface; it is designed to work with front-end 
// neuroscience libraries in other languages, such as the Mathematica Neurotica`Registration 
// namespace of the Neurotica library (see https://github.com/noahbenson/Neurotica/).
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

package nben.mesh.registration;

import nben.mesh.registration.AnchorPotential;
import nben.mesh.registration.EdgePotential;
import nben.mesh.registration.AnglePotential;

import nben.mesh.registration.HarmonicFunction;
import nben.mesh.registration.GaussianFunction;
import nben.mesh.registration.LennardJonesFunction;
import nben.mesh.registration.InfiniteWellFunction;

/** The Fields class is basically a static class that serves as an interface to the potential field
 *  classes of the nben mesh registration library. This is primarily intended for use with 
 *  higher-level langauge interfaces.
 *
 *  @author Noah C. Benson
 */
public final class Fields {

   /** Fields.newHarmonicEdgePotential(s, q, E, X) yields an EdgePotential object with a 
    *  HarmonicFunction form using scale parameter s/m and shape parameter q where where m is the
    *  number of edges. If the parameter E is actually a list of faces, it is interpreted 
    *  automatically.
    *
    *  @param scale The scale or scales of the harmonic function forms (default: 1.0)
    *  @param shape The shape or shapes of the harmonic function forms (default: 2.0)
    *  @params E the edge or face matrix (must be 2 or 3 x m; m is the number of edges or faces)
    *  @params X the reference coordinates for the potential field
    *  @return a new EdgePotential object with the given parameters
    */
   public static EdgePotential newHarmonicEdgePotential(double[] scale, double[] shape,
                                                        int[][] E, double[][] X) {
      if (E.length == 3)
         return newHarmonicEdgePotential(scale, shape, Util.facesToEdges(E), X);
      int n = scale.length;
      HarmonicFunction[] fs = new HarmonicFunction[n];
      for (int i = 0; i < n; ++i)
         fs[i] = new HarmonicFunction(scale[i] / n, shape[i]);
      return new EdgePotential(fs, E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double[] scale, double shape,
                                                        int[][] E, double[][] X) {
      if (E.length == 3)
         return newHarmonicEdgePotential(scale, shape, Util.facesToEdges(E), X);
      int n = scale.length;
      HarmonicFunction[] fs = new HarmonicFunction[n];
      for (int i = 0; i < n; ++i)
         fs[i] = new HarmonicFunction(scale[i] / n, shape);
      return new EdgePotential(fs, E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double[] scale, Double shape,
                                                        int[][] E, double[][] X) {
      return newHarmonicEdgePotential(scale, shape.doubleValue(), E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double scale, double[] shape,
                                                        int[][] E, double[][] X) {
      if (E.length == 3)
         return newHarmonicEdgePotential(scale, shape, Util.facesToEdges(E), X);
      int n = shape.length;
      HarmonicFunction[] fs = new HarmonicFunction[n];
      for (int i = 0; i < n; ++i)
         fs[i] = new HarmonicFunction(scale / n, shape[i]);
      return new EdgePotential(fs, E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(Double scale, double[] shape,
                                                        int[][] E, double[][] X) {
      return newHarmonicEdgePotential(scale.doubleValue(), shape, E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double[] scale, 
                                                        int[][] E, double[][] X) {
      if (E.length == 3)
         return newHarmonicEdgePotential(scale, 2.0, Util.facesToEdges(E), X);
      int n = scale.length;
      HarmonicFunction[] fs = new HarmonicFunction[n];
      for (int i = 0; i < n; ++i)
         fs[i] = new HarmonicFunction(scale[i] / n, 2.0);
      return new EdgePotential(fs, E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double scale, double shape,
                                                        int[][] E, double[][] X) {
      if (E.length == 3) {
         int[][] tmp = Util.facesToEdges(E);
         return newHarmonicEdgePotential(scale, shape, tmp, X);
      } else
         return new EdgePotential(new HarmonicFunction(scale / E[0].length, shape), E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(Double scale, Double shape,
                                                        int[][] E, double[][] X) {
      return newHarmonicEdgePotential(scale.doubleValue(), shape.doubleValue(), E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(double scale, int[][] E, double[][] X) {
      if (E.length == 3) {
         int[][] tmp = Util.facesToEdges(E);
         return newHarmonicEdgePotential(scale, tmp, X);
      } else
         return new EdgePotential(new HarmonicFunction(scale / E[0].length), E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(Double scale, int[][] E, double[][] X) {
      return newHarmonicEdgePotential(scale.doubleValue(), E, X);
   }
   public static EdgePotential newHarmonicEdgePotential(int[][] E, double[][] X) {
      if (E.length == 3)
         return newHarmonicEdgePotential(Util.facesToEdges(E), X);
      else
         return new EdgePotential(new HarmonicFunction(1.0 / E[0].length), E, X);
   }

   /** Fields.newLJEdgePotential(s, q, E, X) yields an EdgePotential object with a 
    *  LennardJonesFunction form using scale parameter s/m and shape parameter q where where m is
    *  the number of edges.
    *
    *  @param scale the constant scale of the Lennard-Jones function (default: 1.0)
    *  @param shape the shape of the Lennard-Jones function (default: 2.0)
    *  @param E the edge or face matrix (must be 2 or 3 x m; m is the number of edges or faces)
    *  @param X the reference coordinates of the potential field
    *  @return a new edge-based potential field with a Lennard-Jones shape and the given parameters
    */
   public static EdgePotential newLJEdgePotential(double[] scale, double[] shape,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, shape, Util.facesToEdges(E), X);
      LennardJonesFunction[] ljs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < ljs.length; ++i)
         ljs[i] = new LennardJonesFunction(scale[i] / E[0].length, shape[i]);
      return new EdgePotential(ljs, E, X);
   }
   public static EdgePotential newLJEdgePotential(double[] scale, double shape,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, shape, Util.facesToEdges(E), X);
      LennardJonesFunction[] ljs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < ljs.length; ++i)
         ljs[i] = new LennardJonesFunction(scale[i] / E[0].length, shape);
      return new EdgePotential(ljs, E, X);
   }
   public static EdgePotential newLJEdgePotential(double[] scale, Double shape,
                                                  int[][] E, double[][] X) {
      return newLJEdgePotential(scale, shape.doubleValue(), E, X);
   }
   public static EdgePotential newLJEdgePotential(double scale, double[] shape,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, shape, Util.facesToEdges(E), X);
      LennardJonesFunction[] ljs = new LennardJonesFunction[shape.length];
      for (int i = 0; i < ljs.length; ++i)
         ljs[i] = new LennardJonesFunction(scale / E[0].length, shape[i]);
      return new EdgePotential(ljs, E, X);
   }
   public static EdgePotential newLJEdgePotential(Double scale, double[] shape,
                                                  int[][] E, double[][] X) {
      return newLJEdgePotential(scale.doubleValue(), shape, E, X);
   }
   public static EdgePotential newLJEdgePotential(double[] scale,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, Util.facesToEdges(E), X);
      LennardJonesFunction[] ljs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < ljs.length; ++i)
         ljs[i] = new LennardJonesFunction(scale[i] / E[0].length);
      return new EdgePotential(ljs, E, X);
   }
   public static EdgePotential newLJEdgePotential(double scale, double shape,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, shape, Util.facesToEdges(E), X);
      else
         return new EdgePotential(new LennardJonesFunction(scale / E[0].length, shape), E, X);
   }
   public static EdgePotential newLJEdgePotential(Double scale, Double shape,
                                                  int[][] E, double[][] X) {
      return newLJEdgePotential(scale.doubleValue(), shape.doubleValue(), E, X);
   }
   public static EdgePotential newLJEdgePotential(double scale,
                                                  int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(scale, Util.facesToEdges(E), X);
      else
         return new EdgePotential(new LennardJonesFunction(scale / E[0].length), E, X);
   }
   public static EdgePotential newLJEdgePotential(Double scale, int[][] E, double[][] X) {
      return newLJEdgePotential(scale.doubleValue(), E, X);
   }
   public static EdgePotential newLJEdgePotential(int[][] E, double[][] X) {
      if (E.length == 3) 
         return newLJEdgePotential(Util.facesToEdges(E), X);
      else
         return new EdgePotential(new LennardJonesFunction(1.0 / E[0].length), E, X);
   }

   /** Fields.newHarmonicAnglePotential(s, q, T, X) yields an AnglePotential object with a
    *  HarmonicFunction form using scale parameter s/m and shape parameter q where where m is
    *  the number of angles (triangles times 3). Angle wells always have a center of Pi/4 and a 
    *  width of Pi/4.
    *
    *  @param scale the scale of the harmonic function form (default: 1.0)
    *  @param shape the shape of the harmonic function form (default: 2.0)
    *  @param T the matrix of triangles (note: this is converted to angles automatically)
    *  @param X the reference coordinates for the potential field
    *  @return a new AnglePotential object with the given parameters
    */
   public static AnglePotential newHarmonicAnglePotential(double[] scale, double[] shape,
                                                          int[][] T, double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / (3 * T[0].length), shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(double[] scale, double shape,
                                                          int[][] T, double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / (3 * T[0].length), shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(double[] scale, Double shape,
                                                          int[][] T, double[][] X) {
      return newHarmonicAnglePotential(scale, shape.doubleValue(), T, X);
   }
   public static AnglePotential newHarmonicAnglePotential(double scale, double[] shape,
                                                          int[][] T, double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[shape.length];
      for (int i = 0; i < shape.length; ++i)
         fs[i] = new HarmonicFunction(scale / (3 * T[0].length), shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(Double scale, double[] shape,
                                                          int[][] T, double[][] X) {
      return newHarmonicAnglePotential(scale.doubleValue(), shape, T, X);
   }
   public static AnglePotential newHarmonicAnglePotential(double[] scale,
                                                          int[][] T, double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / (3 * T[0].length));
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(double scale, double shape,
                                                          int[][] T, double[][] X) {
      return new AnglePotential(new HarmonicFunction(scale / (3.0 * T[0].length), shape), 
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(Double scale, Double shape,
                                                          int[][] T, double[][] X) {
      return newHarmonicAnglePotential(scale.doubleValue(), shape.doubleValue(), T, X);
   }
   public static AnglePotential newHarmonicAnglePotential(double scale,
                                                          int[][] T, double[][] X) {
      return new AnglePotential(new HarmonicFunction(scale / (3.0 * T[0].length)), 
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newHarmonicAnglePotential(Double scale, int[][] T, double[][] X) {
      return newHarmonicAnglePotential(scale.doubleValue(), T, X);
   }
   public static AnglePotential newHarmonicAnglePotential(int[][] T, double[][] X) {
      return new AnglePotential(new HarmonicFunction(1.0 / (3.0 * T[0].length)), 
                                Util.facesToAngles(T), X);
   }

   /** Fields.newLJAnglePotential(s, q, T, X) yields an AnglePotential object with a
    *  LennardJonesFunction form using scale parameter s/m and shape parameter q where where m is
    *  the number of angles (triangles times 3). Angle wells always have a center of Pi/4 and a 
    *  width of Pi/4.
    *
    *  @param scale the scale of the Lennard-Jones function form (default: 1.0)
    *  @param shape the shape of the Lennard-Jones function form (default: 2.0)
    *  @param T the matrix of triangles (note: this is converted to angles automatically)
    *  @param X the reference coordinates for the potential field
    *  @return a new AnglePotential object with the given parameters
    */
   public static AnglePotential newLJAnglePotential(double[] scale, double[] shape,
                                                    int[][] T, double[][] X) {
      LennardJonesFunction[] fs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new LennardJonesFunction(scale[i] / (3 * T[0].length), shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(double[] scale, double shape,
                                                    int[][] T, double[][] X) {
      LennardJonesFunction[] fs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new LennardJonesFunction(scale[i] / (3 * T[0].length), shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(double[] scale, Double shape,
                                                    int[][] T, double[][] X) {
      return newLJAnglePotential(scale, shape.doubleValue(), T, X);
   }
   public static AnglePotential newLJAnglePotential(double scale, double[] shape,
                                                    int[][] T, double[][] X) {
      LennardJonesFunction[] fs = new LennardJonesFunction[shape.length];
      for (int i = 0; i < shape.length; ++i)
         fs[i] = new LennardJonesFunction(scale / (3 * T[0].length), shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(Double scale, double[] shape,
                                                    int[][] T, double[][] X) {
      return newLJAnglePotential(scale.doubleValue(), shape, T, X);
   }
   public static AnglePotential newLJAnglePotential(double[] scale,
                                                    int[][] T, double[][] X) {
      LennardJonesFunction[] fs = new LennardJonesFunction[scale.length];
      for (int i = 0; i < scale.length; ++i)
         fs[i] = new LennardJonesFunction(scale[i] / (3 * T[0].length));
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(double scale, double shape,
                                                    int[][] T, double[][] X) {
      return new AnglePotential(new LennardJonesFunction(scale / (3.0 * T[0].length), shape), 
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(Double scale, Double shape,
                                                    int[][] T, double[][] X) {
      return newLJAnglePotential(scale.doubleValue(), shape.doubleValue(), T, X);
   }
   public static AnglePotential newLJAnglePotential(double scale,
                                                    int[][] T, double[][] X) {
      return new AnglePotential(new LennardJonesFunction(scale / (3.0 * T[0].length)), 
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newLJAnglePotential(Double scale, int[][] T, double[][] X) {
      return newLJAnglePotential(scale.doubleValue(), T, X);
   }
   public static AnglePotential newLJAnglePotential(int[][] T, double[][] X) {
      return new AnglePotential(new LennardJonesFunction(1.0 / (3.0 * T[0].length)), 
                                Util.facesToAngles(T), X);
   }

   /** Fields.newWellAnglePotential(s, q, mn, mx, T, X) yields an AnglePotential object with an
    *  InfiniteWellFunction form using scale parameter s/m, shape parameter q, min value mn, and max
    *  value mx, where where m is the number of angles (triangles times 3).
    *
    *  @param scale the scale of the infinite well function form (default: 1.0)
    *  @param shape the shape of the infinite well function form (default: 0.5)
    *  @param min the minimum value the angle can take (default: 0.0)
    *  @param min the maximum value the angle can take (default: Pi/2)
    *  @param T the matrix of triangles (note: this is converted to angles automatically)
    *  @param X the reference coordinates for the potential field
    *  @return a new AnglePotential object with the given parameters
    */
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      double[] min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min[i], max[i], shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      double[] min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min[i], max, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      double[] min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape, min, max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      double min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min, max[i], shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      Double min, double[] max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape, min.doubleValue(), max, T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      double min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min, max, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      Double min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape, min.doubleValue(), max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double shape,
                                                      double[] min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min[i], max[i], shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, Double shape,
                                                      double[] min, double[] max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape.doubleValue(), min, max, T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double shape,
                                                      double[] min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min[i], max, shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, Double shape,
                                                      double[] min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape.doubleValue(), min, max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double shape,
                                                      double min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min, max[i], shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, Double shape,
                                                      Double min, double[] max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape.doubleValue(), min.doubleValue(), max, T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double shape,
                                                      double min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), min, max, shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, Double shape,
                                                      Double min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape.doubleValue(), 
                                   min.doubleValue(), max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double[] shape,
                                                      double[] min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale / (3 * T[0].length), min[i], max[i], shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, double[] shape,
                                                      double[] min, double[] max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape, min, max, T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double[] shape,
                                                      double[] min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale / (3 * T[0].length), min[i], max, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, double[] shape,
                                                      double[] min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape, min, max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double[] shape,
                                                      double min, double[] max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale / (3 * T[0].length), min, max[i], shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, double[] shape,
                                                      Double min, double[] max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape, min.doubleValue(), max, T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double[] shape,
                                                      double min, double max,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale / (3 * T[0].length), min, max, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, double[] shape,
                                                      Double min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape, 
                                   min.doubleValue(), max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double shape,
                                                      double min, double max,
                                                      int[][] T, double[][] X) {
      return new AnglePotential(new InfiniteWellFunction(scale / (3*T[0].length), min, max, shape),
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, Double shape,
                                                      Double min, Double max,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape.doubleValue(), 
                                   min.doubleValue(), max.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double[] shape,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), 0.0, Math.PI, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, double shape,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), 0.0, Math.PI, shape);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, Double shape,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale, shape.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double[] shape,
                                                      int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale / (3 * T[0].length), 0.0, Math.PI, shape[i]);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, double[] shape,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape, T, X);
   }
   public static AnglePotential newWellAnglePotential(double[] scale, int[][] T, double[][] X) {
      InfiniteWellFunction[] fs = new InfiniteWellFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new InfiniteWellFunction(scale[i] / (3 * T[0].length), 0.0, Math.PI);
      return new AnglePotential(Util.faceFunctionsToAngleFunctions(fs), Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(double scale, double shape,
                                                      int[][] T, double[][] X) {
      return new AnglePotential(new InfiniteWellFunction(scale / (3*T[0].length), 
                                                         0.0, Math.PI, shape),
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale, Double shape,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), shape.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(double scale,
                                                      int[][] T, double[][] X) {
      return new AnglePotential(new InfiniteWellFunction(scale / (3*T[0].length), 0.0, Math.PI),
                                Util.facesToAngles(T), X);
   }
   public static AnglePotential newWellAnglePotential(Double scale,
                                                      int[][] T, double[][] X) {
      return newWellAnglePotential(scale.doubleValue(), T, X);
   }
   public static AnglePotential newWellAnglePotential(int[][] T, double[][] X) {
      return new AnglePotential(new InfiniteWellFunction(1.0 / (3*T[0].length), 0.0, Math.PI),
                                Util.facesToAngles(T), X);
   }

   /** newHarmonicAnchorPotential(s, q, vertices, anchorPoints, X) yields an AnchorPotential object
    *  that operates over the given list of vertices, each of which is attracted to the
    *  corresponding anchor point in the (dims x m)-sized matrix, anchorPoints, where m is the
    *  length of the vertices list. The scale used for the harmonic is s/vertices.length and the
    *  shape is q.
    *
    *  @param scale the scale of the HarmonicFunction form (default: 1.0)
    *  @param shape the shape of the HarmonicFunction form (default: 2.0)
    *  @param vertices the list of vertex IDs that are tied to anchor points
    *  @param points the (d x n)-sized matrix of anchors to which the vertices are drawn; d should
    *                be the dimensionality of the embedding space and n should be the number of
    *                vertices in the vertices parameter
    *  @param X the reference coordinate matrix for the potential
    */
   public static AnchorPotential newHarmonicAnchorPotential(double[] scale, double[] shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / vertices.length, shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double[] scale, double shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / vertices.length, shape);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double[] scale, Double shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return newHarmonicAnchorPotential(scale, shape.doubleValue(), vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double scale, double[] shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[shape.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new HarmonicFunction(scale / vertices.length, shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(Double scale, double[] shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return newHarmonicAnchorPotential(scale.doubleValue(), shape, vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double[] scale,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      HarmonicFunction[] fs = new HarmonicFunction[scale.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new HarmonicFunction(scale[i] / vertices.length);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double scale, double shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new HarmonicFunction(scale / vertices.length, shape),
                                 vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(Double scale, Double shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return newHarmonicAnchorPotential(scale.doubleValue(), shape.doubleValue(), vertices,
                                        points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(double scale, 
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new HarmonicFunction(scale/vertices.length, 2.0),
                                 vertices, points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(Double scale,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return newHarmonicAnchorPotential(scale.doubleValue(), vertices,
                                        points, X);
   }
   public static AnchorPotential newHarmonicAnchorPotential(int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new HarmonicFunction(1.0/vertices.length, 2.0),
                                 vertices, points, X);
   }

   /** newGaussianAnchorPotentials(k, s, q, vertices, anchorPoints, X) yields an AnchorPotential
    *  object that operates over the given list of vertices, each of which is attracted to the 
    *  corresponding anchor point in the (dims x m)-sized matrix, anchorPoints, where m is the
    *  length of the vertices list. The scale used for the inverted Gaussian potential shape is 
    *  k/vertices.length, the Gaussian's standard deviation is s, and the shape is q.
    *
    *  @param scale the scale of the Gaussian function form (default: 1.0)
    *  @param sig the standard deviation (sigma) of the Gaussian function form (default: 1.0)
    *  @param shape the shape of the Gaussian function form (default: 2.0)
    *  @param vertices the list of vertex IDs that are tied to anchor points
    *  @param points the (d x n)-sized matrix of anchors to which the vertices are drawn; d should
    *                be the dimensionality of the embedding space and n should be the number of
    *                vertices in the vertices parameter
    *  @param X the reference coordinate matrix for the potential
    */
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double[] sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig[i], shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double[] sig, 
                                                            double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig[i], shape);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double[] sig, 
                                                            Double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale, sig, shape.doubleValue(), vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig, shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, Double sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale, sig.doubleValue(), shape, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double[] sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale/vertices.length, sig[i], shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, double[] sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig, shape, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale/vertices.length, sig, shape[i]);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, Double sig, 
                                                            double[] shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig.doubleValue(), shape, vertices,
                                        points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double[] sig, 
                                                            double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale/vertices.length, sig[i], shape);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, double[] sig, 
                                                            Double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig, shape.doubleValue(), vertices,
                                        points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double sig, 
                                                            double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig, shape);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, Double sig, 
                                                            Double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale, sig.doubleValue(), shape.doubleValue(), vertices,
                                        points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double sig, double shape,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new GaussianFunction(scale/vertices.length, sig, shape),
                                 vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, Double sig, 
                                                            Double shape,
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig.doubleValue(), shape.doubleValue(),
                                        vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double[] sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig[i], 2.0);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, double sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale[i]/vertices.length, sig, 2.0);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] scale, Double sig, 
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale, sig.doubleValue(),
                                        vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double[] sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(scale/vertices.length, sig[i], 2.0);
      return new AnchorPotential(fs, vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, double[] sig, 
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig,
                                        vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double scale, double sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new GaussianFunction(scale/vertices.length, sig, 2.0),
                                 vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double scale, Double sig, 
                                                            int[] vertices, double[][] points,
                                                            double[][] X) {
      return newGaussianAnchorPotential(scale.doubleValue(), sig.doubleValue(),
                                        vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(double[] sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      GaussianFunction[] fs = new GaussianFunction[vertices.length];
      for (int i = 0; i < fs.length; ++i)
         fs[i] = new GaussianFunction(1.0/vertices.length, sig[i], 2.0);
      return new AnchorPotential(fs, vertices, points, X);
   }

   public static AnchorPotential newGaussianAnchorPotential(double sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new GaussianFunction(1.0/vertices.length, sig, 2.0),
                                 vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(Double sig,
                                                            int[] vertices, double[][] points, 
                                                            double[][] X) {
      return newGaussianAnchorPotential(sig.doubleValue(), vertices, points, X);
   }
   public static AnchorPotential newGaussianAnchorPotential(int[] vertices, double[][] points, 
                                                            double[][] X) {
      return new AnchorPotential(new GaussianFunction(1.0/vertices.length, 1.0, 2.0),
                                 vertices, points, X);
   }

   /** newHarmonicPerimeterPotential(scale, shape, faces, X0) yields a potential function that
    *  includes a harmonic perimeter potential function with the given scale and shape (see
    *  HarmonicFunction). This perimeter prevents vertices around the perimeter of the mesh from
    *  moving very much. The scale passed to the HarmonicFunction is scale / q where q is the number
    *  of vertices in the perimeter.
    *
    *  @param scale the scale of the Harmonic function to use (default: 1.0)
    *  @param shape the shape of the Harmonic function to use (default: 2.0)
    *  @return an AnchorPotential field for the perimeter of the mesh with the given parameters
    */
   public static AnchorPotential newHarmonicPerimeterPotential(double scale, double shape,
                                                               int[][] faces, double[][] X) {
      int[] perim = Util.perimeter(faces);
      double[][] perimX0 = new double[X.length][perim.length];
      for (int j = 0; j < X.length; ++j)
         for (int i = 0; i < perim.length; ++i)
            perimX0[j][i] = X[j][perim[i]];
      return new AnchorPotential(new HarmonicFunction(scale/perim.length, shape), 
                                 perim, perimX0, X);
   }
   public static AnchorPotential newHarmonicPerimeterPotential(Double scale, Double shape,
                                                               int[][] faces, double[][] X) {
      return newHarmonicPerimeterPotential(scale.doubleValue(), shape.doubleValue(), faces, X);
   }
   public static AnchorPotential newHarmonicPerimeterPotential(double scale,
                                                               int[][] faces, double[][] X) {
      return newHarmonicPerimeterPotential(scale, 2.0, faces, X);
   }
   public static AnchorPotential newHarmonicPerimeterPotential(Double scale,
                                                               int[][] faces, double[][] X) {
      return newHarmonicPerimeterPotential(scale.doubleValue(), faces, X);
   }
   public static AnchorPotential newHarmonicPerimeterPotential(int[][] faces, double[][] X) {
      return newHarmonicPerimeterPotential(1.0, 2.0, faces, X);
   }

   /** newStandardMeshPotential(Se, Sa, faces, X0) yields a SumPotential that includes:
    *    (a) an angle potential for the angles in the mesh [infinite-well: scale = Sa/p, min = 0,
    *        max = pi/2, shape = 0.5]
    *    (b) an edge potential for all edges in the given set of faces [harmonic: scale = Se/m, 
    *        shape = 2]
    *
    *  @param edgeScale (default: 250) the scale of the edge potential
    *  @param angleScale (default: 1) the scale of the angle potential
    *  @param faces the triangle matrix of the mesh
    *  @param X the reference coordinate matrix for the mesh
    *  @return a PotentialSum object for the given mesh that maintains edges, angles, and perimeter
    *          positions
    */
   public static PotentialSum newStandardMeshPotential(double edgeScale, double angleScale, 
                                                       int[][] faces, double[][] X) {
      IPotentialField Pa = newWellAnglePotential(angleScale, faces, X);
      IPotentialField Pe = newHarmonicEdgePotential(edgeScale, Util.facesToEdges(faces), X);
      PotentialSum P = new PotentialSum(Pa, Pe);
      if (X.length == 2)
         P.addField(newHarmonicPerimeterPotential(1.0, 2.0, faces, X));
      return P;
   }
   public static PotentialSum newStandardMeshPotential(Double edgeScale, Double angleScale, 
                                                       int[][] faces, double[][] X) {
      return newStandardMeshPotential(edgeScale.doubleValue(), angleScale.doubleValue(), faces, X);
   }
   public static PotentialSum newStandardMeshPotential(double edgeScale, 
                                                       int[][] faces, double[][] X) {
      return newStandardMeshPotential(edgeScale, 1.0, faces, X);
   }
   public static PotentialSum newStandardMeshPotential(Double edgeScale, 
                                                       int[][] faces, double[][] X) {
      return newStandardMeshPotential(edgeScale.doubleValue(), faces, X);
   }
   public static PotentialSum newStandardMeshPotential(int[][] faces, double[][] X) {
      return newStandardMeshPotential(250.0, 1.0, faces, X);
   }

   /** newSum(fields) yields a new potential field object that is the sum of the given lsit of
    *  fields.
    */
   public static PotentialSum newSum() {
      return new PotentialSum();
   }

}
