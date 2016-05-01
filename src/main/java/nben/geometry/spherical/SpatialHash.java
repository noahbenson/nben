////////////////////////////////////////////////////////////////////////////////////////////////////
// SpatialHash.java
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
import nben.geometry.spherical.lib;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Arrays;
import java.util.Iterator;

/** SpatialHash is a simple class that keeps track of meshes defined over spherical surfaces.
 *  Its primary purpose is to enable quick lookup of the triangle in a mesh that contains a point,
 *  or to calculate overlap of shapes on the mesh.
 *  The mesh represents data internally as (x,y,z) coordinate vectors, but all data is assumed to
 *  be on the surface of a sphere; accordingly, all 3D cartesian data passed to SphereMeshHash is
 *  automatically normalized.
 *
 *  @author Noah C. Benson
 */
public class SpatialHash {
   /** hash.coordinates is an N x 3 matrix of coordinates which is referenced by the member 
    *  variables points, greatCircles, ccwArcs, cwArcs, and triangles.
    */
   public final double[][] coordinates;
   /** The list of point-indices in the hash. */
   public final int[] points;
   /** The n x 2 list of coordinates that pair into great circles in the hash. */
   public final int[][] greatCircles;
   /** The n x 2 list of counter-clockwise arc endpoints in the hash. */
   public final int[][] ccwArcs;
   /** The n x 2 list of clockwise arc endpoints in the hash. */
   public final int[][] cwArcs;
   /** The n x 3 list of triangle endpoints in the hash. */
   public final int[][] triangles;
   /** maxContents gives the max number of entities in a partition before it is divided */
   public final int maxContents;
   /** The root partition of the hash. */
   public final Partition root;

   /** The triangles that make up the octants. */
   private static final Triangle[] octantTriangles = new Triangle[] {
      Triangle._from( Point._from( 1,  0, 0),   Point._from( 0,  1, 0),   Point._from(0, 0,  1) ),
      Triangle._from( Point._from( 0,  1, 0),   Point._from(-1,  0, 0),   Point._from(0, 0,  1) ),
      Triangle._from( Point._from(-1,  0, 0),   Point._from( 0, -1, 0),   Point._from(0, 0,  1) ),
      Triangle._from( Point._from( 0, -1, 0),   Point._from( 1,  0, 0),   Point._from(0, 0,  1) ),
      Triangle._from( Point._from( 1,  0, 0),   Point._from( 0, -1, 0),   Point._from(0, 0, -1) ),
      Triangle._from( Point._from( 0, -1, 0),   Point._from(-1,  0, 0),   Point._from(0, 0, -1) ),
      Triangle._from( Point._from(-1,  0, 0),   Point._from( 0,  1, 0),   Point._from(0, 0, -1) ),
      Triangle._from( Point._from( 0,  1, 0),   Point._from( 1,  0, 0),   Point._from(0, 0, -1) )};

   private static final int[] integersToInts(List<Integer> l) {
      if (l == null || l.size() == 0) return null;
      int[] res = new int[l.size()];
      int i = 0;
      for (Iterator<Integer> it = l.iterator(); it.hasNext(); )
         res[i++] = it.next().intValue();
      return res;
   }
   private static final int[][] reconstructElements(List<Integer> l, int[][] from) {
      int[][] res = new int[l.size()][];
      int i = 0;
      for (Iterator<Integer> it = l.iterator(); it.hasNext(); )
         res[i++] = from[it.next().intValue()];
      return res;
   }
   private static final int[] reconstructElements(List<Integer> l, int[] from) {
      int[] res = new int[l.size()];
      int i = 0;
      for (Iterator<Integer> it = l.iterator(); it.hasNext(); )
         res[i++] = from[it.next().intValue()];
      return res;
   }

   private final int[][] greatCircleIntersection(Point pt) {
      if (greatCircles.length == 0) return null;
      List<Integer> igcs = new ArrayList<Integer>();
      GreatCircle gc;
      for (int i = 0; i < greatCircles.length; ++i) {
         gc = GreatCircle._from(coordinates[greatCircles[i][0]],
                                coordinates[greatCircles[i][1]]);
         if (gc.contains(pt)) igcs.add(Integer.valueOf(i));
      }
      return reconstructElements(igcs, greatCircles);
   }

   // Iterators for our elements...
   public interface SpatialHashIterator {
      public int index();
   }
   public final class PointIterator implements Iterator<Point> {
      private int i;
      private PointIterator() {i = 0;}
      public boolean hasNext() {return points != null && i < points.length;}
      public Point next() {return Point._from(coordinates[points[i++]]);}
      public void remove() {throw new UnsupportedOperationException();}
   }
   public final class GreatCircleIterator implements Iterator<GreatCircle> {
      private int i;
      private GreatCircleIterator() {i = 0;}
      public boolean hasNext() {return greatCircles != null && i < greatCircles.length;}
      public GreatCircle next() {
         int[] ids = greatCircles[i++];
         return GreatCircle._from(coordinates[ids[0]], coordinates[ids[1]]);
      }
      public void remove() {throw new UnsupportedOperationException();}
   }
   public final class ArcIterator implements Iterator<Arc> {
      private int i;
      private ArcIterator() {i = 0;}
      public boolean hasNext() {
         return i < (  (cwArcs == null ? 0 : cwArcs.length)
                     + (ccwArcs == null? 0 : ccwArcs.length));
      }
      public Arc next() {
         int[] ids;
         if (ccwArcs == null) {
            ids = cwArcs[i++];
            return Arc._cw(coordinates[ids[0]], coordinates[ids[1]]);
         } else {
            if (i < ccwArcs.length) {
               ids = ccwArcs[i++];
               return Arc._ccw(coordinates[ids[0]], coordinates[ids[1]]);
            } else {
               ids = cwArcs[(i++) - ccwArcs.length];
               return Arc._cw(coordinates[ids[0]], coordinates[ids[1]]);
            }
         }
      }
      public void remove() {throw new UnsupportedOperationException();}
   }
   public final class TriangleIterator implements Iterator<Triangle> {
      private int i;
      private TriangleIterator() {i = 0;}
      public boolean hasNext() {return triangles != null && i < triangles.length;}
      public Triangle next() {
         int[] ids = triangles[i++];
         return Triangle._from(coordinates[ids[0]], coordinates[ids[1]], coordinates[ids[2]]);
      }
      public void remove() {throw new UnsupportedOperationException();}
   }

   /** hash.iteratePoints() yields an iterator over all the points in the given hash */
   public final PointIterator iteratePoints() {return new PointIterator();}
   /** hash.iterateArcs() yields an iterator over all the arcs in the given hash */
   public final ArcIterator iterateArcs() {return new ArcIterator();}
   /** hash.iterateGreatCircless() yields an iterator over all the points in the given hash */
   public final GreatCircleIterator iterateGreatCircles() {return new GreatCircleIterator();}
   /** hash.iterate() yields an iterator over all the points in the given hash */
   public final TriangleIterator iterateTriangles() {return new TriangleIterator();}

   /** hash.point(k) ields the k'th point element in the hash. */
   public final Point point(int k) {
      if (k < 0 || k > points.length) return null;
      return Point._from(coordinates[k]);
   }
   /** hash.ccwArc(k) ields the k'th counter-clockwise arc element in the hash. */
   public final Arc ccwArc(int k) {
      if (k < 0 || k > ccwArcs.length) return null;
      int[] id = ccwArcs[k];
      return Arc._ccw(coordinates[id[0]], coordinates[id[1]]);
   }
   /** hash.cwArc(k) ields the k'th clockwise arc element in the hash. */
   public final Arc cwArc(int k) {
      if (k < 0 || k > cwArcs.length) return null;
      int[] id = cwArcs[k];
      return Arc._cw(coordinates[id[0]], coordinates[id[1]]);
   }
   /** hash.triangle(k) ields the k'th triangle element in the hash. */
   public final Triangle triangle(int k) {
      if (k < 0 || k > triangles.length) return null;
      int[] id = triangles[k];
      return Triangle._from(coordinates[id[0]], coordinates[id[1]], coordinates[id[2]]);
   }

   /** hash.intersection(p) yields a SpatialHash that contains only the spherical elements that 
    *  intersect the given point p. If there is no intersection, null is returned.
    */
   public final SpatialHash intersection(Point p) {
      Map<Integer,Integer> ipts  = new HashMap<Integer,Integer>();
      Map<Integer,Integer> iccws = new HashMap<Integer,Integer>();
      Map<Integer,Integer> icws  = new HashMap<Integer,Integer>();
      Map<Integer,Integer> itris = new HashMap<Integer,Integer>();
      List<Integer> lpts  = new ArrayList<Integer>();
      List<Integer> lccws = new ArrayList<Integer>();
      List<Integer> lcws  = new ArrayList<Integer>();
      List<Integer> ltris = new ArrayList<Integer>();
      Partition newRoot = root.intersection(p, this, 
                                            ipts, iccws, icws, itris,
                                            lpts, lccws, lcws, ltris);
      if (newRoot == null) return null;
      return new SpatialHash(coordinates,
                             reconstructElements(lpts, points),
                             greatCircleIntersection(p),
                             reconstructElements(lccws, ccwArcs),
                             reconstructElements(lcws, cwArcs),
                             reconstructElements(ltris, triangles),
                             maxContents,
                             newRoot);
   }
   /** hash.triangleContainerID(p) yields the first triangle found that contains the given point p.
    *  This method is designed specifically for use with triangle meshes, in which each point p
    *  has 1-n triangle containers depending on whether it is in a triangle, on the boundary of
    *  two triangles, or the vertex of a triangle. This merely returns one of these for the given
    *  point, or null if the point does not lie in a triangle.
    *  Note that this function returns the index of the triangle.
    */
   public final int triangleContainerID(Point p) {
      Map<Integer,Integer> itris = new HashMap<Integer,Integer>();
      List<Integer> ltris = new ArrayList<Integer>();
      Partition newRoot = root.intersection(p, this, 
                                            null, null, null, itris,
                                            null, null, null, ltris);
      if (newRoot == null) return -1;
      return ltris.iterator().next().intValue();
   }
   /** hash.triangleContainerID(p) yields the first triangle found that contains the given point p.
    *  This method is designed specifically for use with triangle meshes, in which each point p
    *  has 1-n triangle containers depending on whether it is in a triangle, on the boundary of
    *  two triangles, or the vertex of a triangle. This merely returns one of these for the given
    *  point, or null if the point does not lie in a triangle.
    */
   public final Triangle triangleContainer(Point p) {
      return triangle(triangleContainerID(p));
   }

   protected SpatialHash(double[][] coords, 
                         int[] pts, int[][] gcs, int[][] ccws, int[][] cws, int[][] tris,
                         int mx,
                         Partition prt) {
      coordinates = coords;
      points       = (pts  == null ? new int[0]    : pts);
      greatCircles = (gcs  == null ? new int[0][0] : gcs);
      ccwArcs      = (ccws == null ? new int[0][0] : ccws);
      cwArcs       = (cws  == null ? new int[0][0] : cws);
      triangles    = (tris == null ? new int[0][0] : tris);
      maxContents  = mx;
      root = (prt == null? new Partition(this) : prt);
   }

   // split a triangle into 4 subtriangles
   public static final Triangle[] splitTriangle(Triangle t) {
      // we can construct this by taking the midpoint of each side and making four triangles out of
      // the original vertices plus these midpoints
      Point midAB = t.arcAB().midpoint();
      Point midBC = t.arcBC().midpoint();
      Point midCA = t.arcCA().midpoint();
      Triangle[] parts = new Triangle[4];
      parts[0] = Triangle._from(t.A, midAB, midCA);
      parts[1] = Triangle._from(t.B, midBC, midAB);
      parts[2] = Triangle._from(t.C, midCA, midBC);
      parts[3] = Triangle._from(midAB, midBC, midCA);
      return parts;
   }

   // the class that stores the sphere partitions
   public static final class Partition {
      // the triangle that contains this partition
      public final Triangle boundary;

      // points, arcs, etc. that lie in this partition
      public final int[] pts;
      public final int[] ccws;
      public final int[] cws;
      public final int[] tris;

      // children partitions; children are always 4 in number and are numbered by the vertex 0-2
      // they are closest to followed by partition 3, the center partition
      public final Partition[] children;

      // used by the intersection function below to keep track of things
      private final static Integer findIntFor(Map<Integer,Integer> h, List<Integer> l, int j) {
         Integer r = Integer.valueOf(j);
         Integer q = h.get(r);
         if (q == null) {
            q = Integer.valueOf(l.size());
            h.put(r, q);
            l.add(r);
         }
         return q;
      }

      // select finds anything whose signed distance is <= d0 of the given point
      public final Partition intersection(Point p,
                                          SpatialHash core,
                                          Map<Integer,Integer> hpts,
                                          Map<Integer,Integer> hccws,
                                          Map<Integer,Integer> hcws,
                                          Map<Integer,Integer> htris,
                                          List<Integer> lpts,
                                          List<Integer> lccws,
                                          List<Integer> lcws,
                                          List<Integer> ltris) {
         // if the point isn't inside our boundary, we can stop here
         if (boundary != null && boundary.relation_to(p) != 1) return null;
         int i, j;
         Partition[] newChildren = null;
         // we want to, first, pass down the query to our childrenn
         if (children != null) {
            Partition[] tmp = new Partition[children.length];
            j = 0;
            for (i = 0; i < children.length; ++i) {
               if (children[i] != null) {
                  tmp[i] = children[i].intersection(p, core,
                                                    hpts, hccws, hcws, htris, 
                                                    lpts, lccws, lcws, ltris);
                  if (tmp[i] != null) ++j;
               }
            }
            if (j == 0) newChildren = null;
            else {
               newChildren = new Partition[j];
               j = 0;
               for (i = 0; i < tmp.length && j < newChildren.length; ++i)
                  if (tmp[i] != null) newChildren[j++] = tmp[i];
            }
         }
         // okay, now we check our own members...
         Arc arc;
         Triangle tri;
         List<Integer> mypts  = new ArrayList<Integer>();
         List<Integer> myccws = new ArrayList<Integer>();
         List<Integer> mycws  = new ArrayList<Integer>();
         List<Integer> mytris = new ArrayList<Integer>();
         if (pts != null && hpts != null) {
            for (i = 0; i < pts.length; ++i) {
               j = pts[i];
               if (Num.eq(p.coords, core.coordinates[core.points[j]]))
                  mypts.add(findIntFor(hpts, lpts, j));
            }
         }
         if (ccws != null && hccws != null) {
            for (i = 0; i < ccws.length; ++i) {
               j = ccws[i];
               arc = Arc._ccw(core.coordinates[core.ccwArcs[j][0]], 
                              core.coordinates[core.ccwArcs[j][1]]);
               if (arc.contains(p))
                  myccws.add(findIntFor(hccws, lccws, j));
            }
         }
         if (cws != null && hcws != null) {
            for (i = 0; i < cws.length; ++i) {
               j = cws[i];
               arc = Arc._cw(core.coordinates[core.cwArcs[j][0]],
                             core.coordinates[core.cwArcs[j][1]]);
               if (arc.contains(p))
                  mycws.add(findIntFor(hcws, lcws, j));
            }
         }
         if (tris != null && htris != null) {
            for (i = 0; i < tris.length; ++i) {
               j = tris[i];
               tri = Triangle._from(core.coordinates[core.triangles[j][0]],
                                    core.coordinates[core.triangles[j][1]],
                                    core.coordinates[core.triangles[j][2]]);
               if (tri.contains(p))
                  mytris.add(findIntFor(htris, ltris, j));
            }
         }
         // last step is to make the new partition:
         // we may not have anything in this particular partition...
         if (mytris.isEmpty() && myccws.isEmpty() && mycws.isEmpty() && mypts.isEmpty()) {
            // if we have no children, just return null
            if (newChildren == null) return null;
            // we have children, but if there's just one, return it
            else if (newChildren.length == 1) return newChildren[0];
            // otherwise, we have to make a new simple partition
            else return new Partition(boundary, null, null, null, null, newChildren);
         }
         // otherwise, we have some amount of data and some children, just make the partition:
         return new Partition(boundary,
                              integersToInts(mypts),
                              integersToInts(myccws),
                              integersToInts(mycws),
                              integersToInts(mytris),
                              newChildren);
      }

      // partitioning data stored as we perform partitioning of the sphere surface
      private abstract class BuildData implements Runnable {
         public SpatialHash              core;
         public Triangle[]               boundaries;
         public int[]                    idcs;
         public ArrayList<List<Integer>> contents;
         public List<Integer>            leftover;

         public BuildData(SpatialHash c, int[] is, Triangle[] bounds) {
            core = c;
            idcs = is;
            boundaries = bounds;
            // we greate a generic array here that we use appropriately
            contents = new ArrayList<List<Integer>>(bounds.length);
            for (int i = 0; i < bounds.length; ++i)
               contents.add(i, new ArrayList<Integer>());
            leftover = new ArrayList<Integer>();
         }
         public BuildData(SpatialHash c, int sz, Triangle[] bounds) {
            this(c, Num.range(sz), bounds);
         }
         // BuildData objects can be run (for parallelization via nben.util.Par);
         // this methods sorts the BuildData elements (idcs) into those in particular
         // triangle boundaries or those that overlap multiple boundaries
         public final void run() {
            int i, j, id, rel;
            // if there are few enough elements, don't subdivide...
            if (idcs == null) {
               return;
            } else if (idcs.length < core.maxContents) {
               for (j = 0; j < idcs.length; ++j) leftover.add(Integer.valueOf(idcs[j]));
            } else {
               // we look through each element and see how it measures up to the partitions
               for (i = 0; i < idcs.length; ++i) {
                  id = idcs[i];
                  // find a triangle that contains it...
                  for (j = 0; j < boundaries.length; ++j) {
                     rel = relation(boundaries[j], id);
                     if (rel > 0) {
                        // it's entirely inside this triangle; we are done...
                        contents.get(j).add(Integer.valueOf(id));
                        break;
                     } else if (rel == 0) {
                        // it's on the boundary or straddles it, we keep it in the outer partition
                        leftover.add(Integer.valueOf(id));
                        break;
                     }
                  }
                  if (j == boundaries.length)
                     throw new IllegalStateException(
                        "element " + id + " in class " + this.getClass().getName()
                        + " had no container partition");
               }
            }
         }
         // this function must be overloaded; a return value of 1 means that the appropriate object
         // for the given index is entirely in the given triangle; a result of 0 means it is on the
         // boundary or straddles the boundary (neither entirely in or out of the triangle), and a 
         // result of -1 indicates that it is entirely outside the triangle.
         abstract public int relation(Triangle t, int idx);
         // convert this build data to a list of partition contents, after the run() above; if any
         // partition contains no elements, then null is returned for that partition
         public final int[][] freeze() {
            int[][] res = new int[boundaries.length][];
            for (int i = 0; i < res.length; ++i) {
               if (contents != null && contents.get(i).size() > 0)
                  res[i] = integersToInts(contents.get(i));
               else
                  res[i] = null;
            }
            return res;
         }
      }
      private class BuildDataPoints extends BuildData {
         public BuildDataPoints(SpatialHash c, int[] is, Triangle[] bounds) {super(c, is, bounds);}
         public int relation(Triangle t, int idx) {
            return t.relation_to(Point._from(core.coordinates[core.points[idx]]));
         }
      }
      private class BuildDataCCWArcs extends BuildData {
         public BuildDataCCWArcs(SpatialHash c, int[] is, Triangle[] bounds) {super(c, is, bounds);}
         public int relation(Triangle t, int idx) {
            return t.relation_to(Arc._ccw(core.coordinates[core.ccwArcs[idx][0]],
                                          core.coordinates[core.ccwArcs[idx][1]]));
         }
      }
      private class BuildDataCWArcs extends BuildData {
         public BuildDataCWArcs(SpatialHash c, int[] is, Triangle[] bounds) {super(c, is, bounds);}
         public int relation(Triangle t, int idx) {
            return t.relation_to(Arc._cw(core.coordinates[core.cwArcs[idx][0]],
                                         core.coordinates[core.cwArcs[idx][1]]));
         }
      }
      private class BuildDataTriangles extends BuildData {
         public BuildDataTriangles(SpatialHash c, int[] is, Triangle[] bounds) {
            super(c, is, bounds);
         }
         public int relation(Triangle t, int idx) {
            return t.relation_to(Triangle._from(core.coordinates[core.triangles[idx][0]],
                                                core.coordinates[core.triangles[idx][1]],
                                                core.coordinates[core.triangles[idx][2]]));
         }
      }
      
      // construct from parent:
      private Partition(SpatialHash core, Triangle t,
                        int[] pts0, int[] ccws0, int[] cws0, int[] tris0) {
         boundary = t;
         Triangle[] parts = (t == null? octantTriangles : splitTriangle(t));
         // we start out with a list of everything (though this will diminish...)
         BuildDataPoints    bdpts  = new BuildDataPoints   (core, pts0,  parts);
         BuildDataCCWArcs   bdccws = new BuildDataCCWArcs  (core, ccws0, parts);
         BuildDataCWArcs    bdcws  = new BuildDataCWArcs   (core, cws0,  parts);
         BuildDataTriangles bdtris = new BuildDataTriangles(core, tris0, parts);

         // run these...
         bdpts  = new BuildDataPoints(    core, pts0,  parts);
         bdccws = new BuildDataCCWArcs(   core, ccws0, parts);
         bdcws  = new BuildDataCWArcs(    core, cws0,  parts);
         bdtris = new BuildDataTriangles( core, tris0, parts);
         bdpts.run();
         bdccws.run();
         bdcws.run();
         bdtris.run();
         pts  = integersToInts(bdpts.leftover);
         ccws = integersToInts(bdccws.leftover);
         cws  = integersToInts(bdcws.leftover);
         tris = integersToInts(bdtris.leftover);
         int[][] 
            finpts  = bdpts.freeze(),
            finccws = bdccws.freeze(),
            fincws  = bdcws.freeze(),
            fintris = bdtris.freeze();
         // make the children...
         Partition[] chldn = new Partition[parts.length];
         int c = 0;
         for (int i = 0; i < chldn.length; ++i) {
            if ((finpts[i] == null || finpts[i].length == 0)
                && (finccws[i] == null || finccws[i].length == 0)
                && (fincws[i] == null || fincws[i].length == 0)
                && (fintris[i] == null || fintris[i].length == 0))
               chldn[i] = null;
            else {
               chldn[i] = new Partition(core, parts[i],
                                        finpts[i], finccws[i], fincws[i], fintris[i]);
               ++c;
            }
         }
         if (c == 0) children = null;
         else children = chldn;
         // that's all!
      }
      // constructs a new partition for the SpatialHash root
      private Partition(SpatialHash core) {
         this(core, null, 
              Num.range(core.points    == null? 0 : core.points.length),
              Num.range(core.ccwArcs   == null? 0 : core.ccwArcs.length),
              Num.range(core.cwArcs    == null? 0 : core.cwArcs.length),
              Num.range(core.triangles == null? 0 : core.triangles.length));
      }
      // construct a partition quickly when we know what goes in it
      private Partition(Triangle t,
                        int[] pts0, int[] ccws0, int[] cws0, int[] tris0,
                        Partition[] chldn) {
         boundary = t;
         pts = pts0;
         ccws = ccws0;
         cws = cws0;
         tris = tris0;
         children = chldn;
      }
   }

   /** SpatialHash.from(coords, pointIndices, arcIndices, triangleIndices) yields a new spatial hash
    *  containing the points indexed by coords[i] for i in pointIndices, the ccw arcs indexed by 
    *  (coord[arcIndices[i][0]], coord[arcIndices[i][1]]), and the triangles similarly indexed by
    *  the n x 3 trianglesIndices matrix. Not all elements in coords needs to be used. This method
    *  copies its arguments.
    */
   public static final SpatialHash from(double[][] coords, int[] pts, int[][] ccws, int[][] tris) {
      // fix up the coordinates matrix:
      if (!Num.is_matrix(coords) || coords[0].length != 3)
         throw new IllegalArgumentException("coords matrix must be n x 3");
      int n = coords.length, i;
      double[][] newCoords = new double[n][];
      for (i = 0; i < n; ++i) newCoords[i] = Num.normalized(coords[i]);
      // fix up pts:
      int[] newPts = null;
      if (pts != null) {
         pts = new int[pts.length];
         for (i = 0; i < pts.length; ++i)
            if (pts[i] >= n) throw new IllegalArgumentException("pointIndex[" + i + "] out of range");
            else newPts[i] = pts[i];
      }
      // fix up ccws:
      int[][] newCcws = null;
      if (ccws != null) {
         ccws = new int[ccws.length][2];
         for (i = 0; i < ccws.length; ++i)
            if (ccws[i][0] >= n || ccws[i][1] >= n || ccws[i][0] < 0 || ccws[i][1] < 0)
               throw new IllegalArgumentException("arcIndex[" + i + "] out of range.");
            else {
               newCcws[i][0] = ccws[i][0];
               newCcws[i][1] = ccws[i][1];
            }
      }
      // fix up tris:
      int[][] newTris = null;
      if (tris != null) {
         newTris = new int[tris.length][3];
         for (i = 0; i < tris.length; ++i)
            if (tris[i][0] < 0 || tris[i][1] < 0 || tris[i][2] < 0 || 
                tris[i][0] >= n || tris[i][1] >= n || tris[i][2] >= n)
               throw new IllegalArgumentException("triangleIndex[" + i + "] out of range.");
            else {
               newTris[i][0] = tris[i][0];
               newTris[i][1] = tris[i][1];
               newTris[i][2] = tris[i][2];
            }
      }
      // make the hash:
      return _from(newCoords, newPts, newCcws, newTris);
   }
   /** SpatialHash._from(coords, pts, arcs, tris) is equivalent to the from version of the function
    *  except that it doesn't check or copy its arguments.
    */
   public static final SpatialHash _from(double[][] coords, int[] pts, int[][] ccws, int[][] tris) {
      return new SpatialHash(coords, pts, null, ccws, null, tris, 64, null);
   }
}
