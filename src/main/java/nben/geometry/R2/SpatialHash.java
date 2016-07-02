////////////////////////////////////////////////////////////////////////////////////////////////////
// SpatialHash.java
//
// The nben.geometry.R2 namespace contains code used to represent and compute over planar geometryn
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
import nben.geometry.spherical.lib;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Arrays;
import java.util.Iterator;

/** SpatialHash is a simple class that keeps track of meshes defined in the plane.
 *  Its primary purpose is to enable quick lookup of the triangle in a mesh that contains a point,
 *  or to calculate overlap of shapes on the mesh.
 *
 *  @author Noah C. Benson
 */
public class SpatialHash {
   /** hash.coordinates is an N x 2 matrix of coordinates which is referenced by the member 
    *  variables points, lines, segments, and triangles.
    */
   public final double[][] coordinates;
   /** The list of point-indices in the hash. */
   public final int[] points;
   /** The n x 2 list of coordinates that pair into lines in the hash. */
   public final int[][] lines;
   /** The n x 2 list of line segment endpoints in the hash. */
   public final int[][] segments;
   /** The n x 3 list of triangle endpoints in the hash. */
   public final int[][] triangles;
   /** maxContents gives the max number of entities in a partition before it is divided */
   public final int maxContents;
   /** The root partition of the hash. */
   public final Partition root;

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

   private final int[][] lineIntersection(Point pt) {
      if (lines.length == 0) return null;
      List<Integer> igcs = new ArrayList<Integer>();
      Line gc;
      for (int i = 0; i < lines.length; ++i) {
         gc = Line._from(coordinates[lines[i][0]], coordinates[lines[i][1]]);
         if (gc.contains(pt)) igcs.add(Integer.valueOf(i));
      }
      return reconstructElements(igcs, lines);
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
   public final class LineIterator implements Iterator<Line> {
      private int i;
      private LineIterator() {i = 0;}
      public boolean hasNext() {return lines != null && i < lines.length;}
      public Line next() {
         int[] ids = lines[i++];
         return Line._from(coordinates[ids[0]], coordinates[ids[1]]);
      }
      public void remove() {throw new UnsupportedOperationException();}
   }
   public final class LineSegmentIterator implements Iterator<LineSegment> {
      private int i;
      private LineSegmentIterator() {i = 0;}
      public boolean hasNext() {return segments != null && i < segments.length;}
      public LineSegment next() {
         int[] ids = segments[i++];
         return LineSegment._from(coordinates[ids[0]], coordinates[ids[1]]);
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
   /** hash.iterateLineSegments() yields an iterator over all the arcs in the given hash */
   public final LineSegmentIterator iterateLineSegments() {return new LineSegmentIterator();}
   /** hash.iterateLiness() yields an iterator over all the points in the given hash */
   public final LineIterator iterateLines() {return new LineIterator();}
   /** hash.iterate() yields an iterator over all the points in the given hash */
   public final TriangleIterator iterateTriangles() {return new TriangleIterator();}

   /** hash.point(k) ields the k'th point element in the hash. */
   public final Point point(int k) {
      if (k < 0 || k > points.length) return null;
      return Point._from(coordinates[k]);
   }
   /** hash.segment(k) yields the k'th line segment element in the hash. */
   public final LineSegment segment(int k) {
      if (k < 0 || k > segments.length) return null;
      int[] id = segments[k];
      return LineSegment._from(coordinates[id[0]], coordinates[id[1]]);
   }
   /** hash.triangle(k) ields the k'th triangle element in the hash. */
   public final Triangle triangle(int k) {
      if (k < 0 || k > triangles.length) return null;
      int[] id = triangles[k];
      return Triangle._from(coordinates[id[0]], coordinates[id[1]], coordinates[id[2]]);
   }

   /** hash.intersection(p) yields a SpatialHash that contains only the planar elements that 
    *  intersect the given point p. If there is no intersection, null is returned.
    */
   public final SpatialHash intersection(Point p) {
      Map<Integer,Integer> ipts  = new HashMap<Integer,Integer>();
      Map<Integer,Integer> isegs = new HashMap<Integer,Integer>();
      Map<Integer,Integer> itris = new HashMap<Integer,Integer>();
      List<Integer> lpts  = new ArrayList<Integer>();
      List<Integer> lsegs = new ArrayList<Integer>();
      List<Integer> ltris = new ArrayList<Integer>();
      Partition newRoot = root.intersection(p, this, 
                                            ipts, isegs, itris,
                                            lpts, lsegs, ltris);
      if (newRoot == null) return null;
      return new SpatialHash(coordinates,
                             reconstructElements(lpts, points),
                             lineIntersection(p),
                             reconstructElements(lsegs, segments),
                             reconstructElements(ltris, triangles),
                             maxContents,
                             newRoot);
   }
   /** hash.triangleContainerID(p) yields the first triangle found that contains the given point p.
    *  This method is designed specifically for use with triangle meshes, in which each point p
    *  has 1-n triangle containers depending on whether it is in a triangle, on the boundary of
    *  two triangles, or the vertex of a triangle. This merely returns one of these for the given
    *  point, or -1 if the point does not lie in a triangle.
    *  Note that this function returns the index of the triangle.
    */
   public final int triangleContainerID(Point p) {
      Map<Integer,Integer> itris = new HashMap<Integer,Integer>();
      List<Integer> ltris = new ArrayList<Integer>();
      Partition newRoot = root.intersection(p, this, 
                                            null, null, itris,
                                            null, null, ltris);
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
      int tri = triangleContainerID(p);
      return (tri == -1? null : triangle(tri));
   }

   protected SpatialHash(double[][] coords, 
                         int[] pts, int[][] lns, int[][] segs, int[][] tris,
                         int mx,
                         Partition prt) {
      coordinates = coords;
      points      = (pts  == null ? new int[0]    : pts);
      lines       = (lns  == null ? new int[0][0] : lns);
      segments    = (segs == null ? new int[0][0] : segs);
      triangles   = (tris == null ? new int[0][0] : tris);
      maxContents = mx;
      root = (prt == null? new Partition(this) : prt);
   }

   // the class that stores the sphere partitions
   public static final class Partition {
      // How we define the relevant quadrant
      public final Rectangle boundary;

      // points, segments, etc. that lie in this partition
      public final int[] pts;
      public final int[] segs;
      public final int[] tris;

      // children partitions; children are always 4 in number and are numbered in the traditional
      // quadrant order
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
                                          Map<Integer,Integer> hsegs,
                                          Map<Integer,Integer> htris,
                                          List<Integer> lpts,
                                          List<Integer> lsegs,
                                          List<Integer> ltris) {
         // if the point isn't inside our boundary, we can stop here
         if (p.coords[0] < boundary.lowerLeft.coords[0]  ||
             p.coords[1] < boundary.lowerLeft.coords[1]  ||
             p.coords[0] > boundary.upperRight.coords[0] ||
             p.coords[1] > boundary.upperRight.coords[1])
            return null;
         int i, j;
         Partition[] newChildren = null;
         // we want to, first, pass down the query to our childrenn
         if (children != null) {
            Partition[] tmp = new Partition[children.length];
            j = 0;
            for (i = 0; i < children.length; ++i) {
               if (children[i] != null) {
                  tmp[i] = children[i].intersection(p, core,
                                                    hpts, hsegs, htris, 
                                                    lpts, lsegs, ltris);
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
         LineSegment seg;
         Triangle tri;
         List<Integer> mypts  = new ArrayList<Integer>();
         List<Integer> mysegs = new ArrayList<Integer>();
         List<Integer> mytris = new ArrayList<Integer>();
         if (pts != null && hpts != null) {
            for (i = 0; i < pts.length; ++i) {
               j = pts[i];
               if (Num.eq(p.coords, core.coordinates[core.points[j]]))
                  mypts.add(findIntFor(hpts, lpts, j));
            }
         }
         if (segs != null && segs != null) {
            for (i = 0; i < segs.length; ++i) {
               j = segs[i];
               seg = LineSegment._from(core.coordinates[core.segments[j][0]],
                                       core.coordinates[core.segments[j][1]]);
               if (seg.contains(p))
                  mysegs.add(findIntFor(hsegs, lsegs, j));
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
         if (mytris.isEmpty() && mysegs.isEmpty() && mypts.isEmpty()) {
            // if we have no children, just return null
            if (newChildren == null) return null;
            // we have children, but if there's just one, return it
            else if (newChildren.length == 1) return newChildren[0];
            // otherwise, we have to make a new simple partition
            else return new Partition(boundary, null, null, null, newChildren);
         }
         // otherwise, we have some amount of data and some children, just make the partition:
         return new Partition(boundary,
                              integersToInts(mypts),
                              integersToInts(mysegs),
                              integersToInts(mytris),
                              newChildren);
      }

      // partitioning data stored as we perform partitioning of the sphere surface
      private abstract class BuildData implements Runnable {
         public SpatialHash              core;
         public Rectangle[]              boundaries;
         public int[]                    idcs;
         public ArrayList<List<Integer>> contents;
         public List<Integer>            leftover;

         public BuildData(SpatialHash c, int[] is, Rectangle[] bounds) {
            core = c;
            idcs = is;
            boundaries = bounds;
            // we greate a generic array here that we use appropriately
            contents = new ArrayList<List<Integer>>(bounds.length);
            for (int i = 0; i < bounds.length; ++i)
               contents.add(i, new ArrayList<Integer>());
            leftover = new ArrayList<Integer>();
         }
         public BuildData(SpatialHash c, int sz, Rectangle[] bounds) {
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
         // for the given index is entirely in the given rectangle; a result of 0 means it is on the
         // boundary or straddles the boundary (neither entirely in or out of the rectangle), and a 
         // result of -1 indicates that it is entirely outside the rectangle.
         abstract public int relation(Rectangle t, int idx);
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
         public BuildDataPoints(SpatialHash c, int[] is, Rectangle[] bounds) {super(c, is, bounds);}
         public int relation(Rectangle t, int idx) {
            return t.relation_to(Point._from(core.coordinates[core.points[idx]]));
         }
      }
      private class BuildDataLineSegments extends BuildData {
         public BuildDataLineSegments(SpatialHash c, int[] is, Rectangle[] bounds) {
            super(c, is, bounds);
         }
         public int relation(Rectangle t, int idx) {
            return t.relation_to(LineSegment._from(core.coordinates[core.segments[idx][0]],
                                                   core.coordinates[core.segments[idx][1]]));
         }
      }
      private class BuildDataTriangles extends BuildData {
         public BuildDataTriangles(SpatialHash c, int[] is, Rectangle[] bounds) {
            super(c, is, bounds);
         }
         public int relation(Rectangle t, int idx) {
            return t.relation_to(Triangle._from(core.coordinates[core.triangles[idx][0]],
                                                core.coordinates[core.triangles[idx][1]],
                                                core.coordinates[core.triangles[idx][2]]));
         }
      }

      private static final Rectangle[] split(Rectangle r, double[] mid) {
         Point p = Point._from(mid);
         return new Rectangle[] {
            Rectangle.from(p, r.upperRight),
            Rectangle.from(Point._from(r.lowerLeft.coords[0], p.coords[1]),
                           Point._from(p.coords[0], r.upperRight.coords[1])),
            Rectangle.from(r.lowerLeft, p),
            Rectangle.from(Point._from(p.coords[0], r.lowerLeft.coords[1]),
                           Point._from(r.upperRight.coords[0], p.coords[1]))};
      }
      
      // construct from parent:
      private Partition(SpatialHash core, Rectangle t,
                        int[] pts0, int[] segs0, int[] tris0) {
         boundary = t;
         double[] mid = new double[2];
         int midcount = 0, i;
         if (pts0 != null) for (i = 0; i < pts0.length; ++i) {
               ++midcount;
               mid[0] += core.coordinates[pts0[i]][0];
               mid[1] += core.coordinates[pts0[i]][1];
            }
         if (segs0 != null) for (i = 0; i < segs0.length; ++i) {
               midcount += 2;
               mid[0] += core.coordinates[core.segments[segs0[i]][0]][0];
               mid[1] += core.coordinates[core.segments[segs0[i]][0]][1];
               mid[0] += core.coordinates[core.segments[segs0[i]][1]][0];
               mid[1] += core.coordinates[core.segments[segs0[i]][1]][1];
            }
         if (tris0 != null) for (i = 0; i < tris0.length; ++i) {
               midcount += 3;
               mid[0] += core.coordinates[core.triangles[tris0[i]][0]][0];
               mid[1] += core.coordinates[core.triangles[tris0[i]][0]][1];
               mid[0] += core.coordinates[core.triangles[tris0[i]][1]][0];
               mid[1] += core.coordinates[core.triangles[tris0[i]][1]][1];
               mid[0] += core.coordinates[core.triangles[tris0[i]][2]][0];
               mid[1] += core.coordinates[core.triangles[tris0[i]][2]][1];
            }
         if (midcount > 0) {
            mid[0] /= midcount;
            mid[1] /= midcount;
         }
         Rectangle[] parts = split(t, mid);
         // we start out with a list of everything (though this will diminish...)
         BuildDataPoints       bdpts  = new BuildDataPoints(      core, pts0,  parts);
         BuildDataLineSegments bdsegs = new BuildDataLineSegments(core, segs0, parts);
         BuildDataTriangles    bdtris = new BuildDataTriangles(   core, tris0, parts);

         // run these...
         bdpts  = new BuildDataPoints(      core, pts0,  parts);
         bdsegs = new BuildDataLineSegments(core, segs0, parts);
         bdtris = new BuildDataTriangles(   core, tris0, parts);
         bdpts.run();
         bdsegs.run();
         bdtris.run();
         pts  = integersToInts(bdpts.leftover);
         segs = integersToInts(bdsegs.leftover);
         tris = integersToInts(bdtris.leftover);
         int[][] 
            finpts  = bdpts.freeze(),
            finsegs = bdsegs.freeze(),
            fintris = bdtris.freeze();
         // make the children...
         Partition[] chldn = new Partition[parts.length];
         int c = 0;
         for (i = 0; i < chldn.length; ++i) {
            if ((finpts[i] == null || finpts[i].length == 0)
                && (finsegs[i] == null || finsegs[i].length == 0)
                && (fintris[i] == null || fintris[i].length == 0))
               chldn[i] = null;
            else {
               chldn[i] = new Partition(core, parts[i],
                                        finpts[i], finsegs[i], fintris[i]);
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
              Num.range(core.segments  == null? 0 : core.segments.length),
              Num.range(core.triangles == null? 0 : core.triangles.length));
      }
      // construct a partition quickly when we know what goes in it
      private Partition(Rectangle t,
                        int[] pts0, int[] segs0, int[] tris0,
                        Partition[] chldn) {
         boundary = t;
         pts = pts0;
         segs = segs0;
         tris = tris0;
         children = chldn;
      }
   }

   /** SpatialHash.from(coords, pointIndices, segIndices, triangleIndices) yields a new spatial hash
    *  containing the points indexed by coords[i] for i in pointIndices, the segments are indexed by
    *  (coord[segIndices[i][0]], coord[segIndices[i][1]]), and the triangles similarly indexed by
    *  the n x 3 trianglesIndices matrix. Not all elements in coords needs to be used. This method
    *  copies its arguments.
    */
   public static final SpatialHash from(double[][] coords, int[] pts, int[][] segs, int[][] tris) {
      // fix up the coordinates matrix:
      if (!Num.is_matrix(coords) || coords[0].length != 2)
         throw new IllegalArgumentException("coords matrix must be n x 2");
      int n = coords.length, i;
      double[][] newCoords = new double[n][2];
      for (i = 0; i < n; ++i) {
         newCoords[i][0] = coords[i][0];
         newCoords[i][1] = coords[i][1];
      }
      // fix up pts:
      int[] newPts = null;
      if (pts != null) {
         pts = new int[pts.length];
         for (i = 0; i < pts.length; ++i)
            if (pts[i] >= n) throw new IllegalArgumentException("pointIndex[" + i + "] out of range");
            else newPts[i] = pts[i];
      }
      // fix up segs:
      int[][] newSegs = null;
      if (segs != null) {
         segs = new int[segs.length][2];
         for (i = 0; i < segs.length; ++i)
            if (segs[i][0] >= n || segs[i][1] >= n || segs[i][0] < 0 || segs[i][1] < 0)
               throw new IllegalArgumentException("arcIndex[" + i + "] out of range.");
            else {
               newSegs[i][0] = segs[i][0];
               newSegs[i][1] = segs[i][1];
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
      return _from(newCoords, newPts, newSegs, newTris);
   }
   /** SpatialHash._from(coords, pts, arcs, tris) is equivalent to the from version of the function
    *  except that it doesn't check or copy its arguments.
    */
   public static final SpatialHash _from(double[][] coords, int[] pts, int[][] segs, int[][] tris) {
      return new SpatialHash(coords, pts, null, segs, tris, 64, null);
   }
}
