////////////////////////////////////////////////////////////////////////////////////////////////////
// MeshTopology.java
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

import nben.util.Par;
import nben.util.Num;
import nben.util.Numpy;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

/** A MeshTopology object stores the topological organization of a mesh topology defined in the 2D
 *  plane. A 2D mesh topology is used primarily to represent models in the plane.
 *  
 *  @author Noah C. Benson
 */
public class MeshTopology {
   /** The triangles contained in this mesh; all must be counter-clockwise oriented. */
   public final int[][] triangles;
   /** The number of vertices in this triangle mesh. */
   public final int vertexCount;

   // construct the mesh topology
   private MeshTopology(int[][] tris, int n) {
      triangles = tris;
      vertexCount = n;
   }


   private static final ArrayList<Integer> edgePair(int u, int v) {
      ArrayList<Integer> l = new ArrayList<Integer>(2);
      l.add(Integer.valueOf(u));
      l.add(Integer.valueOf(v));
      return l;
   }
   private static final void trackEdges(Set<ArrayList<Integer>> found,
                                        Set<ArrayList<Integer>> complete,
                                        int u, int v) {
      ArrayList<Integer>
         ef = edgePair(u, v),
         er = edgePair(v, u);
      // if the edge is already in there, that's a problem:
      if (found.contains(ef) || complete.contains(ef))
         throw new IllegalArgumentException("Duplicate edges not allowed in topological meshes");
      // if the reverse edge has been found, we move them both to complete
      else if (found.contains(er)) {
         found.remove(er);
         complete.add(ef);
         complete.add(er);
      // otherwise, this edge is not yet seen; add it to found, to await its reverse pair
      } else
         found.add(ef);
   }
   /** Constructs a mesh topology object from the given byte array (which is converted via the
    *  nben.util.Numpy.int2FromBytes function to an int[][] array).
    */
   public static final MeshTopology fromBytes(byte[] b) {
      return MeshTopology.from(Numpy.int2FromBytes(b));
   }
   /** Constructs a mesh topology object and returns it; if the given arguments do not form a valid
    *  mesh topology, then an exception is thrown. This function makes a copy of tris.
    */
   public static final MeshTopology from(int[][] tris) {
      int maxVertex = 0;
      Set<ArrayList<Integer>> 
         foundEdges    = new HashSet<ArrayList<Integer>>(),
         completeEdges = new HashSet<ArrayList<Integer>>();
      int[] tri;
      int[][] newTris = new int[tris.length][3];
      for (int i = 0; i < tris.length; ++i) {
         tri = tris[i];
         if (tri.length != 3)
            throw new IllegalArgumentException("triangle matrix must be n x 3");
         if (tri[0] < 0 || tri[1] < 0 || tri[2] < 0)
            throw new IllegalArgumentException("triangle matrix must contain only indices >= 0");
         if (tri[0] > maxVertex) maxVertex = tri[0];
         if (tri[1] > maxVertex) maxVertex = tri[1];
         if (tri[2] > maxVertex) maxVertex = tri[2];
         // We just want to make sure every edge appears exactly twice; otherwise, this is not a
         // complete topology. We use the trackEdges function to keep track of these.
         trackEdges(foundEdges, completeEdges, tri[0], tri[1]);
         trackEdges(foundEdges, completeEdges, tri[1], tri[2]);
         trackEdges(foundEdges, completeEdges, tri[2], tri[0]);
         newTris[i][0] = tri[0];
         newTris[i][1] = tri[1];
         newTris[i][2] = tri[2];
      }
      // if any edges were found but not completed, we don't have a complete mesh
      if (foundEdges.size() > 0)
         throw new IllegalArgumentException("Triangle mesh does not contain complements for all" +
                                            " edges thus cannot cover the unit sphere");
      return MeshTopology._from(newTris, maxVertex);
   }
   /** MeshTopology._from(triangles, n) yields a MeshTopology with the given nx3 matrix of triangle
    *  indices (i.e., each element triangles[i][k] is the vertex number of triangle i's k'th vertex;
    *  vertices are 0-indexed). This function performs no error checking, so invalid triangles or
    *  an incorrect max vertex will result in undefined behavior. This function does not copy its
    *  arguments, so triangles should be an array that will be treated immutably.
    */
   public static final MeshTopology _from(int[][] tris, int n) {
      return new MeshTopology(tris, n);
   }
   
   /** The Address class is a simple structure that can be used to store addresses in a mesh;
    *  they describe positions that are isomorphic across registrations.
    */
   public static final class Address {
      /** the ID (index) of the triangle */
      public int id;
      /** the relative x coordinate within the triangle */
      double x;
      /** the relative y coordinate within the triangle */
      double y;
      /** construct an Address */
      public Address(int id, double x, double y) {
         this.id = id;
         this.x = x;
         this.y = y;
      }
   }
   /** The MeshTopology.Registration class is used to store an instance of a topological 
    *  registration: a set of coordinates for the vertices specified in MeshTopology's triangles
    *  member variable. The vertices must not invert any of the triangles from being 
    *  counter-clockwise oriented to being clockwise-oriented.
    */
   public final class Registration {
      /** The coordinates of the vertices in the registration. */
      public final double[][] coordinates;
      /** The spatial hash that is used for addressing points. */
      public final SpatialHash hash;
      // construct the registration (always via register or _register below)
      private Registration(double[][] coords) {
         coordinates = coords;
         hash = SpatialHash._from(coords, null, null, triangles);
      }
      /** reg.lookup(address) yields the Point in the 2D plane that is inside the
       *  triangle indexed by the id and the (x,y) coordinate of the given address.
       */
      public final Point lookup(Address addr) {
         return hash.triangle(addr.id).lookup(addr.x, addr.y);
      }
      /** reg.lookup(triangleID, x, y) yields the Point on the sphere surface that is inside the
       *  triangle indexed by the given triangleID at the position specified by the address (x,y).
       */
      public final Point lookup(int id, double x, double y) {
         return hash.triangle(id).lookup(x, y);
      }
      public final Point lookup(int id, double[] addr) {
         return hash.triangle(id).lookup(addr[0], addr[1]);
      }
      /** reg.address(p) yields the (i, x, y) address of the point p inside the triangle mesh */
      public final Address address(Point p) {
         int id = hash.triangleContainerID(p);
         if (id == -1)
            throw new IllegalStateException("No triangle containing point;" + 
                                            " illegal topology/registration!");
         double[] addr = hash.triangle(id).address(p);
         return new Address(id, addr[0], addr[1]);
      }
   }

   /** topology.register(coords) yields a Registration object for the given coordinates combined 
    *  with the given topology.
    */
   public final Registration register(double[][] coords) {
      if (coords.length < vertexCount)
         throw new IllegalArgumentException("coordinate matrix must be n x 2 where n >= "
                                            + vertexCount);
      return new Registration(coords);
   }
   /** topology._register(coords) yields a Registration object for the given coordinates combined 
    *  with the given topology. This is identical to register but does not check the arguments.
    */
   public final Registration _register(double[][] coords) {
      return new Registration(coords);
   }
   /** topology.registerBytes(b) yields a Registration object for the given coordinates combined 
    *  with the given topology. The coordinates, which must be encoded in the byte array b, are
    *  converted to a double[][] array via the nben.util.Numpy.double2FromBytes() funciton.
    */
   public final Registration registerBytes(byte[] b) {
      return register(Numpy.double2FromBytes(b));
   }

   /** topology.transform(from, to, coords) yields a transformation of the rows of coords from the
    *  topology registration given in from to the registration to.
    */
   public final double[][] transform(Registration from, Registration to, double[][] coords) {
      // get addresses from the from and look them up in to:
      Point p;
      double[][] tx = new double[coords.length][3];
      for (int i = 0; i < coords.length; ++i) {
         p = to.lookup(from.address(Point.from(coords[i])));
         tx[i][0] = p.coords[0];
         tx[i][1] = p.coords[1];
         tx[i][2] = p.coords[2];
      }
      return coords;
   }
   /** topology._transform(from, to, coords) yields a transformation of the rows of coords from the
    *  topology registration given in from to the registration to. This function is identical to the
    *  transform function, except that it does not check arguments.
    */
   public final double[][] _transform(Registration from, Registration to, double[][] coords) {
      // get addresses from the from and look them up in to:
      Point p;
      double[][] tx = new double[coords.length][3];
      for (int i = 0; i < coords.length; ++i) {
         p = to.lookup(from.address(Point._from(coords[i])));
         tx[i][0] = p.coords[0];
         tx[i][1] = p.coords[1];
         tx[i][2] = p.coords[2];
      }
      return coords;
   }

   /** The Interpolator class stores the necessary data for interpolating a value out of an ordered
    *  list of values; it is used by the interpolation methods below:
    */
   public static final class Interpolator {
      /** The indices from which we interpolate. */
      public final int[] indices;
      /** The weights of these verex's values. */
      public final double[] weights;
      /** The value of the index (from indices) with the highest weight. */
      public final int closest;
      /** Construct an interpolator. */
      public Interpolator(int[] idcs, double[] wgts) {
         if (idcs.length != wgts.length)
            throw new IllegalArgumentException("weight and index lengths must match");
         indices = idcs;
         double tmp = 0, highest = wgts[0];
         int cl = indices[0];
         for (int i = 0; i < wgts.length; ++i) {
            tmp += wgts[i];
            if (wgts[i] > highest) {
               highest = wgts[i];
               cl = indices[i];
            }
         }
         closest = cl;
         if (Num.eq(tmp, 1.0)) {
            weights = wgts;
         } else {
            weights = new double[wgts.length];
            for (int i = 0; i < weights.length; ++i)
               weights[i] = wgts[i] / tmp;
         }
      }

      /** Interpolates from the vals and yields the result; ignore any masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A mask value m is considered included
       *  if (m &gt; 0.5).
       */
      public final double interpolate(double[] vals, double[] mask, double nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j] > 0.5) {
                  res += weights[i] * vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return res / tot;
      }
      /** Interpolates from the vals and yields the result; ignore any masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A mask value is considered included
       *  if it is not equal to 0 and excluded if it is equal to 0.
       */
      public final double interpolate(double[] vals, int[] mask, double nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j] != 0) {
                  res += weights[i] * vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return res / tot;
      }
      /** Interpolates from the vals and yields the result; ignore an masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A max value is considered included
       *  if it is true and excluded if it is false.
       */
      public final double interpolate(double[] vals, boolean[] mask, double nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j]) {
                  res += weights[i] * vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return res / tot;
      }

     /** Interpolates from the vals and yields the result; ignore any masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A mask value m is considered included
       *  if (m &gt; 0.5).
       */
      public final int interpolate(int[] vals, double[] mask, int nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * (double)vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j] > 0.5) {
                  res += weights[i] * (double)vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return (int)Math.round(res / tot);
      }
      /** Interpolates from the vals and yields the result; ignore any masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A mask value is considered included
       *  if it is not equal to 0 and excluded if it is equal to 0.
       */
      public final int interpolate(int[] vals, int[] mask, int nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * (double)vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j] != 0) {
                  res += weights[i] * (double)vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return (int)Math.round(res / tot);
      }
      /** Interpolates from the vals and yields the result; ignore an masked vals. If all vals
       *  relevant to a point are masked, set it to nullval. A max value is considered included
       *  if it is true and excluded if it is false.
       */
      public final int interpolate(int[] vals, boolean[] mask, int nullval) {
         double res = 0;
         double tot = 0;
         int i, j;
         if (mask == null) {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               for (i = 0; i < indices.length; ++i) {
                  res += weights[i] * (double)vals[indices[i]];
                  tot += weights[i];
               }
            }
         } else {
            for (i = 0; i < indices.length; ++i) {
               j = indices[i];
               if (mask[j]) {
                  res += weights[i] * (double)vals[j];
                  tot += weights[i];
               }
            }
         }
         if (Num.zeroish(tot)) return nullval;
         else                  return (int)Math.round(res / tot);
      }
      
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(double[] vals, double[] mask) {
         return interpolate(vals, mask, 0.0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(double[] vals, int[] mask) {
         return interpolate(vals, mask, 0.0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(double[] vals, boolean[] mask) {
         return interpolate(vals, mask, 0.0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, null, nullval).
       */
      public final double interpolate(double[] vals, double nullval) {
         return interpolate(vals, (double[])null, nullval);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, null, 0).
       */
      public final double interpolate(double[] vals) {
         return interpolate(vals, (double[])null, 0.0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(int[] vals, double[] mask) {
         return interpolate(vals, mask, 0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(int[] vals, int[] mask) {
         return interpolate(vals, mask, 0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, mask, 0).
       */
      public final double interpolate(int[] vals, boolean[] mask) {
         return interpolate(vals, mask, 0);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, null, nullval).
       */
      public final double interpolate(int[] vals, int nullval) {
         return interpolate(vals, (double[])null, nullval);
      }
      /** Interpolates from the vals and yields the result; equivalent to 
       *  interpolate(vals, null, 0).
       */
      public final double interpolate(int[] vals) {
         return interpolate(vals, (double[])null, 0);
      }

      /** Uses nearest-neighbor interpolation */
      public final double neighbor(double[] vals, double[] mask, double nullval) {
         return (mask == null || mask[closest] > 0.5? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final double neighbor(double[] vals, int[] mask, double nullval) {
         return (mask == null || mask[closest] != 0? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final double neighbor(double[] vals, boolean[] mask, double nullval) {
         return (mask == null || mask[closest]? vals[closest] : nullval);
      }

      /** Uses nearest-neighbor interpolation */
      public final int neighbor(int[] vals, double[] mask, int nullval) {
         return (mask == null || mask[closest] > 0.5? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final int neighbor(int[] vals, int[] mask, int nullval) {
         return (mask == null || mask[closest] != 0? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final int neighbor(int[] vals, boolean[] mask, int nullval) {
         return (mask == null || mask[closest]? vals[closest] : nullval);
      }

      /** Uses nearest-neighbor interpolation */
      public final Object neighbor(Object[] vals, double[] mask, Object nullval) {
         return (mask == null || mask[closest] > 0.5? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final Object neighbor(Object[] vals, int[] mask, Object nullval) {
         return (mask == null || mask[closest] != 0? vals[closest] : nullval);
      }
      /** Uses nearest-neighbor interpolation */
      public final Object neighbor(Object[] vals, boolean[] mask, Object nullval) {
         return (mask == null || mask[closest]? vals[closest] : nullval);
      }

      /** Equivalent to neighbor(vals, mask, 0)  */
      public final double neighbor(double[] vals, double[] mask) {
         return neighbor(vals, mask, 0.0);
      }
      /** Equivalent to neighbor(vals, mask, 0)  */
      public final double neighbor(double[] vals, int[] mask) {
         return neighbor(vals, mask, 0.0);
      }
      /** Equivalent to neighbor(vals, mask, 0)  */
      public final double neighbor(double[] vals, boolean[] mask) {
         return neighbor(vals, mask, 0.0);
      }
      /** Equivalent to neighbor(vals, null, nullval)  */
      public final double neighbor(double[] vals, double nullval) {
         return neighbor(vals, (double[])null, nullval);
      }

      /** Equivalent to neighbor(vals, mask, 0)  */
      public final int neighbor(int[] vals, double[] mask) {
         return neighbor(vals, mask, 0);
      }
      /** Equivalent to neighbor(vals, mask, 0)  */
      public final int neighbor(int[] vals, int[] mask) {
         return neighbor(vals, mask, 0);
      }
      /** Equivalent to neighbor(vals, mask, 0)  */
      public final int neighbor(int[] vals, boolean[] mask) {
         return neighbor(vals, mask, 0);
      }
      /** Equivalent to neighbor(vals, null, nullval)  */
      public final int neighbor(int[] vals, int nullval) {
         return neighbor(vals, (double[])null, nullval);
      }

      /** Equivalent to neighbor(vals, mask, null)  */
      public final Object neighbor(Object[] vals, double[] mask) {
         return neighbor(vals, mask, null);
      }
      /** Equivalent to neighbor(vals, mask, null)  */
      public final Object neighbor(Object[] vals, int[] mask) {
         return neighbor(vals, mask, null);
      }
      /** Equivalent to neighbor(vals, mask, null)  */
      public final Object neighbor(Object[] vals, boolean[] mask) {
         return neighbor(vals, mask, null);
      }
      /** Equivalent to neighbor(vals, null, nullval)  */
      public final Object neighbor(Object[] vals, Object nullval) {
         return neighbor(vals, (double[])null, nullval);
      }
   }
   /** topology.interpolation(from, to, order, near) yields an array of Interpolator objects, one
    *  per vertex in the to registration, such that if m = I[i], then m.interpolate(data), for an
    *  array data of vertex values of the from registration, yields the interpolated value of the
    *  from/data to the to registration. The final argument, order, must be a non-negative integer
    *  and specifies the order of the interpolation. A value of 0 indicates nearest-neighbor
    *  interpolation; 1 indicates linear, 2 quadratic, etc. The near parameter should be set to
    *  true if the nearest point in the mesh to a given coordinate should be used when a point is
    *  outside the triangle mesh; if set to false, then the appropriate array element will be null.
    */
   public final Interpolator[] interpolation(Registration from, double[][] to,
                                             int order, boolean near) {
      int i, j; 
      Interpolator[] interp;
      Point p;
      Triangle tri;
      int[] vtcs;
      double[] weights;
      double tot;
      if (order < 0) throw new IllegalArgumentException("Order must be non-negative");
      else if (order == 0) {
         interp = new Interpolator[to.length];
         double d1, d2, d3;
         weights = new double[] {1.0};
         for (i = 0; i < to.length; ++i) {
            p = Point.from(to[i]);
            j = from.hash.triangleContainerID(p);
            vtcs = from.hash.triangles[j];
            d1 = Num.euclideanDistance2(from.hash.coordinates[vtcs[0]], p.coords);
            d2 = Num.euclideanDistance2(from.hash.coordinates[vtcs[1]], p.coords);
            d3 = Num.euclideanDistance2(from.hash.coordinates[vtcs[2]], p.coords);
            vtcs = new int[] {(d1 <= d2
                               ? (d1 <= d3? vtcs[0] : vtcs[2])
                               : (d2 <= d3? vtcs[1] : vtcs[2]))};
            interp[i] = new Interpolator(vtcs, weights);
         }
      } else if (order == 1) {
         // prep our result
         interp = new Interpolator[to.length];
         for (i = 0; i < to.length; ++i) {
            p = Point.from(to[i]);
            j = from.hash.triangleContainerID(p);
            if (j == -1) {
               if (near) {
                  p = from.hash.nearest(p, true);
                  j = from.hash.triangleContainerID(p);
               }
               if (j == -1) {
                  interp[i] = null;
                  continue;
               }
            }
            vtcs = from.hash.triangles[j];
            // split this triangle at the point p
            tri = Triangle._from(from.hash.coordinates[vtcs[0]],
                                 from.hash.coordinates[vtcs[1]],
                                 from.hash.coordinates[vtcs[2]]);
            tot = tri.area();
            if (Num.zeroish(tot)) {
               interp[i] = new Interpolator(vtcs, new double[] {1.0/3.0, 1.0/3.0, 1.0/3.0});
            } else {
               weights = new double[3];
               weights[0] = Triangle._from(p, tri.B, tri.C).area()/tot;
               weights[1] = Triangle._from(p, tri.C, tri.A).area()/tot;
               weights[2] = Triangle._from(p, tri.A, tri.B).area()/tot;
               interp[i] = new Interpolator(vtcs, weights);
            }
         }
      } else {
         // a simple interpretation of the order parameter
         interp = interpolation(from, to, 1);
         tot = 0;
         for (i = 0; i < interp.length; ++i) {
            weights = interp[i].weights;
            for (j = 0; j < weights.length; j++) {
               weights[i] = Math.pow(weights[i], order);
               tot += weights[i];
            }
            for (j = 0; j < weights.length; j++)
               weights[i] /= tot;
         }
      }
      return interp;
   }
   /** topology.interpolation(from, to, order) is equivalent to 
    *  topology.interpolation(from, to. order, false).
    */
   public final Interpolator[] interpolation(Registration from, double[][] to, int order) {
      return interpolation(from, to, order, false);
   }
   /** topology.interpolation(from, to, near) is equivalent to 
    *  topology.interpolation(from, to. 1, near).
    */
   public final Interpolator[] interpolation(Registration from, double[][] to, boolean near) {
      return interpolation(from, to, 1, near);
   }
   /** topology.interpolation(from, to) is equivalent to 
    *  topology.interpolation(from, to. 1, false).
    */
   public final Interpolator[] interpolation(Registration from, double[][] to) {
      return interpolation(from, to, 1, false);
   }

   public final Interpolator interpolation(Registration from, double[] to,
                                           int order, boolean near) {
      return interpolation(from, new double[][] {to}, order, near)[0];
   }
   /** topology.interpolation(from, to, order) is equivalent to 
    *  topology.interpolation(from, to. order, false).
    */
   public final Interpolator interpolation(Registration from, double[] to, int order) {
      return interpolation(from, to, order, false);
   }
   /** topology.interpolation(from, to, near) is equivalent to 
    *  topology.interpolation(from, to. 1, near).
    */
   public final Interpolator interpolation(Registration from, double[] to, boolean near) {
      return interpolation(from, to, 1, near);
   }
   /** topology.interpolation(from, to) is equivalent to 
    *  topology.interpolation(from, to. 1, false).
    */
   public final Interpolator interpolation(Registration from, double[] to) {
      return interpolation(from, to, 1, false);
   }



   /** topology.interpolate(from, to, data, order, near, mask, nullval) yields an array of values
    *  that have been interpolated from the underlying from mesh to the points in the matrix to. The
    *  given data array is assumed to be the values at the from vertices, and mask is an array of 
    *  either 0 or 1 values indicating whether the equivalent vertex in from is included (1) or not
    *  (0) in the 'valid' region from which to interpolate. This may be null to indicate all 1's.
    *  If a point in to is considered 'invalid' by the mask, then nullval is used instead of the
    *  correct interpolated value. The near parameter is passed directly to the 
    *  topology.interpolation function.
    */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, boolean near, double[] mask, double nullval) {
      Interpolator[] interp = interpolation(from, to, order, near);
      int tmp = 1;
      double[] res = new double[interp.length];
      for (int i = 0; i < interp.length; ++i)
         res[i] = (interp[i] == null? nullval : interp[i].interpolate(data, mask, nullval));
      return res;
   }
   
   /** Uses nullval = 0 */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, boolean near, double[] mask) {
      return interpolate(from, to, data, order, near, mask, 0.0);
   }
   /** Uses mask = null */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, boolean near, double nullval) {
      return interpolate(from, to, data, order, near, (double[])null, nullval);
   }
   /** Uses near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, double[] mask, double nullval) {
      return interpolate(from, to, data, order, false, mask, nullval);
   }
   /** Uses order = 1 */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     boolean near, double[] mask, double nullval) {
      return interpolate(from, to, data, 1, near, mask, nullval);
   }

   /** Uses nullval = 0 and order = 1*/
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     boolean near, double[] mask) {
      return interpolate(from, to, data, 1, near, mask, 0.0);
   }
   /** Uses mask = null and order = 1 */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     boolean near, double nullval) {
      return interpolate(from, to, data, 1, near, (double[])null, nullval);
   }
   /** Uses near = false and order = 1 */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     double[] mask, double nullval) {
      return interpolate(from, to, data, 1, false, mask, nullval);
   }
   /** Uses nullval = 0 and near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, double[] mask) {
      return interpolate(from, to, data, order, false, mask, 0.0);
   }
   /** Uses mask = null and near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, double nullval) {
      return interpolate(from, to, data, order, false, (double[])null, nullval);
   }
   /** Uses mask = null and nullval = 0.0 */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order, boolean near) {
      return interpolate(from, to, data, order, near, (double[])null, 0.0);
   }


   /** Uses nullval = 0 and order = 1 and near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     double[] mask) {
      return interpolate(from, to, data, 1, false, mask, 0.0);
   }
   /** Uses mask = null and order = 1 and near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     double nullval) {
      return interpolate(from, to, data, 1, false, (double[])null, nullval);
   }
   /** Uses nullval = 0 and near = false and mask = null*/
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     int order) {
      return interpolate(from, to, data, order, false, (double[])null, 0.0);
   }
   /** Uses nullval = 0 and order = 1 and mask = null*/
   public final double[] interpolate(Registration from, double[][] to, double[] data,
                                     boolean near) {
      return interpolate(from, to, data, 1, near, (double[])null, 0.0);
   }

   /** uses nullval = 0. order = 1, mask = null, and near = false */
   public final double[] interpolate(Registration from, double[][] to, double[] data) {
      return interpolate(from, to, data, 1, false, (double[])null, 0.0);
   }

   /** topology.interpolate(from, to, data, order, near, mask, nullval) yields a value that has been
    *  interpolated from the underlying from mesh to the point given by the vertex to. The given
    *  data array is assumed to be the values at the from vertices, and mask is an array of 
    *  either 0 or 1 values indicating whether the equivalent vertex in from is included (1) or not
    *  (0) in the 'valid' region from which to interpolate. This may be null to indicate all 1's.
    *  If a point in to is considered 'invalid' by the mask, then nullval is used instead of the
    *  correct interpolated value. The near parameter is passed directly to the 
    *  topology.interpolation function.
    */
   public final double interpolate(Registration from, double[] to, double[] data,
                                     int order, boolean near, double[] mask, double nullval) {
      Interpolator[] interp = interpolation(from, new double[][] {to}, order, near);
      return (interp[0] == null? nullval : interp[0].interpolate(data, mask, nullval));
   }
   
   /** Uses nullval = 0 */
   public final double interpolate(Registration from, double[] to, double[] data,
                                     int order, boolean near, double[] mask) {
      return interpolate(from, to, data, order, near, mask, 0.0);
   }
   /** Uses mask = null */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order, boolean near, double nullval) {
      return interpolate(from, to, data, order, near, (double[])null, nullval);
   }
   /** Uses near = false */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order, double[] mask, double nullval) {
      return interpolate(from, to, data, order, false, mask, nullval);
   }
   /** Uses order = 1 */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   boolean near, double[] mask, double nullval) {
      return interpolate(from, to, data, 1, near, mask, nullval);
   }

   /** Uses nullval = 0 and order = 1*/
   public final double interpolate(Registration from, double[] to, double[] data,
                                   boolean near, double[] mask) {
      return interpolate(from, to, data, 1, near, mask, 0.0);
   }
   /** Uses mask = null and order = 1 */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   boolean near, double nullval) {
      return interpolate(from, to, data, 1, near, (double[])null, nullval);
   }
   /** Uses near = false and order = 1 */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   double[] mask, double nullval) {
      return interpolate(from, to, data, 1, false, mask, nullval);
   }
   /** Uses nullval = 0 and near = false */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order, double[] mask) {
      return interpolate(from, to, data, order, false, mask, 0.0);
   }
   /** Uses mask = null and near = false */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order, double nullval) {
      return interpolate(from, to, data, order, false, (double[])null, nullval);
   }
   /** Uses mask = null and nullval = 0.0 */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order, boolean near) {
      return interpolate(from, to, data, order, near, (double[])null, 0.0);
   }


   /** Uses nullval = 0 and order = 1 and near = false */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   double[] mask) {
      return interpolate(from, to, data, 1, false, mask, 0.0);
   }
   /** Uses mask = null and order = 1 and near = false */
   public final double interpolate(Registration from, double[] to, double[] data,
                                   double nullval) {
      return interpolate(from, to, data, 1, false, (double[])null, nullval);
   }
   /** Uses nullval = 0 and near = false and mask = null*/
   public final double interpolate(Registration from, double[] to, double[] data,
                                   int order) {
      return interpolate(from, to, data, order, false, (double[])null, 0.0);
   }
   /** Uses nullval = 0 and order = 1 and mask = null*/
   public final double interpolate(Registration from, double[] to, double[] data,
                                   boolean near) {
      return interpolate(from, to, data, 1, near, (double[])null, 0.0);
   }

   /** uses nullval = 0. order = 1, mask = null, and near = false */
   public final double interpolate(Registration from, double[] to, double[] data) {
      return interpolate(from, to, data, 1, false, (double[])null, 0.0);
   }

}

