////////////////////////////////////////////////////////////////////////////////////////////////////
// PersistentLabelledHashGraph.java, part of nben, a mathematics library for clojure.
// This file defines the persistent labelled graph implementation using clojure's persistent hash 
// maps.
// 
// Copyright (C) 2012 Noah C. Benson
//
// This file is part of the nben clojure library.
//
// The nben clojure library is free software: you can redistribute it and/or modify it under the 
// terms of the GNU General Public License as published by the Free Software Foundation, either 
// version 3 of the License, or (at your option) any later version.
//
// The nben clojure library is distributed in the hope that it will be useful, but WITHOUT ANY 
// WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with the nben clojure
// library.  If not, see <http://www.gnu.org/licenses/>.
//

package nben.jvm;
import nben.jvm.IPersistentGraph;
import clojure.lang.*;
import java.util.*;
import java.io.Serializable;

////////////////////////////////////////////////////////////////////////////////////////////////////
// INTERNAL IMPLEMENTATION NOTES
// The critical thing about the PersistentLabelledHashGraph, which is a very generic graph object,
// is that it must be indexed very carefully.
// In addition to a set of vertices and a set of edges, it contains two indices:
// The key index (m_kindex) is an index of edge key -> (object-edge index)
// The vertex index (m_vindex) is an index of object -> (key-edge index)
// Objects are anything that is linked by edges in the graph (vertices or edges)
// Keys are:
//   - for a map-edge, the key mapped to the object
//   - for a vector-edge, the Integer index of the object
//   - for a set-edge, null
// Vertices are non-edges that are in the graph; edges are vector, map, or set collections
// An object-edge index maps from an object (which must be part of an edge) to edges containing
//   that object
// A key-edge index maps from an object (which must be part of an edge) to edges containing that
//   key.

/** PersistentLabelledHashGraph is an implementation of the IPersistentGraph interface.  The class
 *  can be treated as a persistent graph but also acts identically to a PersistentHashMap for all
 *  intents and purposes; adding vertices and edges is done exactly as if you were assoc'ing them
 *  into a map, for example.  
 * 
 *  @author Noah C. Benson
 *  @version 1.0
 */
public class PersistentLabelledHashGraph 
   extends AFn
   implements IPersistentGraph, IObj, IEditableCollection, IPersistentMap, Serializable//, IHashEq
{
   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The subclasses of this class
   protected static final class HashGraphSeq extends ASeq
   {
      final static private long serialVersionUID = 1L;
      private final ISeq V;
      private final ISeq E;
      protected HashGraphSeq(ISeq vertices, ISeq edges, IPersistentMap meta)
      {
         super(meta);
         V = vertices;
         E = edges;
      }
      public Object first() 
      {
         return V.first();
      }
      public ISeq next()
      {
         ISeq v = V.next(), e = E;
         if (v == null) {
            v = e;
            e = null;
         }
         if (v == null) return null;
         else return new HashGraphSeq(v, e, meta());
      }
      public HashGraphSeq withMeta(IPersistentMap meta)
      {
         return new HashGraphSeq(V, E, meta);
      }
      static public ISeq create(ISeq V, ISeq E, IPersistentMap meta)
      {
         if (V == null) {
            V = E;
            E = null;
         }
         if (V == null) return null;
         else return new HashGraphSeq(V, E, meta);
      }
   }
   // This class organizes hashes for us and intersects/unions them efficiently
   protected static final class PersistentMapSorter
   {
      private Vector<IPersistentMap> m_dat;
      private IPersistentMap m_smallest;
      private IPersistentMap m_largest;
      private IPersistentMap m_isect;
      private IPersistentMap m_union;

      public PersistentMapSorter(int size)
      {
         m_dat = null;
         reset(size);
      }
      public void reset(int newsize)
      {
         if (newsize < 2) newsize = 4;
         if (m_dat == null) m_dat = new Vector<IPersistentMap>(newsize);
         else {
            m_dat.clear();
            m_dat.ensureCapacity(newsize);
         }
         m_smallest = null;
         m_largest = null;
         m_isect = null;
         m_union = null;
      }
      public void reset()
      {
         reset(m_dat.size());
      }
      public void add(IPersistentMap m)
      {
         if (m != null)
            m_dat.add(m);
         else
            m = PersistentHashMap.EMPTY;
         m_isect = null;
         m_union = null;
         if (m_smallest == null || m.count() < m_smallest.count())
            m_smallest = m;
         if (m_largest == null || m.count() < m_largest.count())
            m_largest = m;
      }
      public int count()
      {
         return m_dat.size();
      }
      public IPersistentMap intersection()
      {
         if (m_isect != null) return m_isect;
         else if (m_dat.size() == 0) return PersistentHashMap.EMPTY;
         else {
            ISeq q;
            IPersistentMap m;
            Object obj;
            int i, sz = m_dat.size();
            m_isect = m_smallest;
            for (q = m_isect.seq(); q != null; q = q.next()) {
               obj = ((MapEntry)q.first()).key();
               for (i = 0; i < sz; ++i) {
                  m = m_dat.elementAt(i);
                  if (m == m_smallest) continue;
                  if (!m.containsKey(obj)) {
                     m_isect = m_isect.without(obj);
                     break;
                  }
               }
            }
            return m_isect;
         }
      }
      public IPersistentMap intersectionSubIntersection()
      {
         // This function is the same as intersection(), but it assumes that we are intersecting
         // maps that contain keys mapped to maps; when we intersect two of these high-level maps,
         // we must assure that the low-level maps are intersected as well.  E.g.,
         // {:a {:b 1 :c 2}, :b {:c 1 :d 2}} intersect {:b {:d 2 :e 3} :f {:a 1 :b 2}}
         //  --> {:b {:d 2 :e 3}} or {:b {:c 1 :d 2}} (either; unspecified)
         // {:a {:b 1 :c 2}, :b {:c 1 :d 2}} intersectSubIntersect {:b {:d 2 :e 3} :f {:a 1 :b 2}}
         //  --> {:b {:d 2}}
         if (m_isect != null) return m_isect;
         else if (m_dat.size() == 0) return PersistentHashMap.EMPTY;
         else {
            // we calculate the intersection by going through the maps in sorted order
            ISeq q;
            IPersistentMap m;
            Object obj;
            MapEntry me;
            int i, sz = m_dat.size();
            PersistentMapSorter pms = new PersistentMapSorter(10);

            m_isect = PersistentHashMap.EMPTY;
            for (q = m_smallest.seq(); q != null; q = q.next()) {
               me = (MapEntry)q.first();
               obj = me.key();
               m = (IPersistentMap)me.val();
               if (m == null) continue;
               pms.reset();
               pms.add(m);
               for (i = 0; i < sz; ++i) {
                  m = m_dat.elementAt(i);
                  if (m == m_smallest) continue;
                  m = (IPersistentMap)m.valAt(obj, null);
                  if (m == null || m.count() == 0) {
                     break;
                  } else
                     pms.add(m);
               }
               if (i == sz) {
                  m = pms.intersection();
                  if (m.count() > 0)
                     // get the intersection...
                     m_isect = m_isect.assoc(obj, m);
               }
            }
            return m_isect;
         }
      }
      //public IPersistentMap intersectionSubUnion()
      //{
      //   // same as the above but uses union instead of intersection of the sub-maps
      //   if (m_isect != null) return m_isect;
      //   else if (m_dat.size() == 0) return PersistentHashMap.EMPTY;
      //   else {
      //      // we calculate the intersection by going through the maps in sorted order
      //      ISeq q;
      //      IPersistentMap m;
      //      Object obj;
      //      MapEntry me;
      //      int i, sz = m_dat.size();
      //      PersistentMapSorter pms = new PersistentMapSorter(10);
      //
      //      m_isect = PersistentHashMap.EMPTY;
      //      for (q = m_smallest.seq(); q != null; q = q.next()) {
      //         me = (MapEntry)q.first();
      //         obj = me.key();
      //         pms.reset();
      //         pms.add((IPersistentMap)me.val());
      //         for (i = 0; i < sz; ++i) {
      //            m = m_dat.elementAt(i);
      //            if (m == m_smallest) continue;
      //            m = (IPersistentMap)m.valAt(obj, null);
      //            if (m == null || m.count() == 0) {
      //               break;
      //            } else
      //               pms.add(m);
      //         }
      //         if (i == sz)
      //            // get the intersection...
      //            m_isect.assoc(obj, pms.union());
      //      }
      //      return m_isect;
      //   }
      //}
      public IPersistentMap union()
      {
         if (m_union != null) return m_union;
         else if (m_dat.size() == 0) return PersistentHashMap.EMPTY;
         else {
            ISeq q;
            IPersistentMap m;
            MapEntry me;
            ITransientMap union = (ITransientMap)((IEditableCollection)m_largest).asTransient();
            for (int i = 0; i < m_dat.size(); ++i) {
               m = m_dat.elementAt(i);
               if (m == m_largest) continue;
               // go through all of these and put them in the largest...
               for (q = m.seq(); q != null; q = q.next()) {
                  me = (MapEntry)q.first();
                  union = union.assoc(me.key(), me.val());
               }
            }
            return (m_union = union.persistent());
         }
      }
   }
   protected static final class TransientLabelledHashGraph extends AFn implements ITransientMap
   {
      private ITransientMap m_vertices;
      private ITransientMap m_edges;
      private ITransientMap m_kindex;
      private ITransientMap m_vindex;
      TransientLabelledHashGraph(PersistentLabelledHashGraph g)
      {
         m_vertices = (ITransientMap)((IEditableCollection)g.m_vertices).asTransient();
         m_edges = (ITransientMap)((IEditableCollection)g.m_edges).asTransient();
         m_kindex = (ITransientMap)((IEditableCollection)g.m_kindex).asTransient();
         m_vindex = (ITransientMap)((IEditableCollection)g.m_vindex).asTransient();
      }
      public final ITransientMap assoc(Object key, Object val)
      {
         IPersistentMap map, tmp;
         Object obj;
         if (key instanceof IPersistentVector) {
            m_edges.assoc(key, val);
            IPersistentVector v = (IPersistentVector)key;
            Integer ii;
            for (int i = 0; i < v.count(); ++i) {
               ii = new Integer(i);
               obj = v.nth(i);
               // vertex index first
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap) {
                  if (m_edges.valAt(obj, this) == this)
                     this.assoc(obj, null);
               } else {
                  if (m_vertices.valAt(obj, this) == this)
                     this.assoc(obj, null);
               }
               tmp = (IPersistentMap)m_vindex.valAt(obj, null);
               if (tmp == null) tmp = PersistentHashMap.EMPTY;
               m_vindex.assoc(obj, tmp.assoc(key, val));
               // key index next
               map = (IPersistentMap)m_kindex.valAt(ii, null);
               if (map == null)
                  map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
               else {
                  tmp = (IPersistentMap)map.valAt(obj);
                  if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
                  else map = map.assoc(obj, tmp.assoc(key, val));
               }
               m_kindex.assoc(ii, map);
            }
         } else if (key instanceof IPersistentSet) {
            m_edges.assoc(key, val);
            IPersistentSet s = (IPersistentSet)key;
            map = (IPersistentMap)m_kindex.valAt(null, null);
            for (ISeq q = s.seq(); q != null; q = q.next()) {
               obj = q.first();
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap) {
                  if (m_edges.valAt(obj, this) == this) {
                     m_kindex.assoc(null, map);
                     this.assoc(obj, null);
                     map = (IPersistentMap)m_kindex.valAt(null, null);
                  }
               } else {
                  if (m_vertices.valAt(obj, this) == this) {
                     m_kindex.assoc(null, map);
                     this.assoc(obj, null);
                     map = (IPersistentMap)m_kindex.valAt(null, null);
                  }
               }
               tmp = (IPersistentMap)m_vindex.valAt(obj, null);
               if (tmp == null) tmp = PersistentHashMap.EMPTY;
               m_vindex.assoc(obj, tmp.assoc(key, val));
               if (map == null)
                  map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
               else {
                  tmp = (IPersistentMap)map.valAt(obj);
                  if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
                  else map = map.assoc(obj, tmp.assoc(key, val));
               }
            }
            m_kindex.assoc(null, map);
         } else if (key instanceof IPersistentMap) {
            m_edges.assoc(key, val);
            IPersistentMap m = (IPersistentMap)key;
            MapEntry me;
            for (ISeq q = m.seq(); q != null; q = q.next()) {
               me = (MapEntry)q.first();
               obj = me.val();
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap) {
                  if (m_edges.valAt(obj, this) == this)
                     m_edges.assoc(obj, null);
               } else {
                  if (m_vertices.valAt(obj, this) == this)
                     m_vertices.assoc(obj, null);
               }
               tmp = (IPersistentMap)m_vindex.valAt(obj, null);
               if (tmp == null) tmp = PersistentHashMap.EMPTY;
               m_vindex.assoc(obj, tmp.assoc(key, val));
               // next, key index
               map = (IPersistentMap)m_kindex.valAt(me.key(), null);
               if (map == null)
                  map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
               else {
                  tmp = (IPersistentMap)map.valAt(obj);
                  if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
                  else map = map.assoc(obj, tmp.assoc(key, val));
               }
               m_kindex.assoc(me.key(), map);
            }
         } else {
            if (m_vertices.valAt(key, this) == this)
               m_vindex.assoc(key, null);
            m_vertices.assoc(key, val);
         }
         return this;
      }
      public final ITransientMap without(Object key)
      {
         Object obj;
         IPersistentMap map, tmp;
         if (key instanceof IPersistentVector) {
            IPersistentVector v = (IPersistentVector)key;
            // the need for the first of these tests is due to a bug in clojure...
            if (m_edges.count() > 0 && m_edges.valAt(v, this) != this) {
               Integer ii;
               m_edges = m_edges.without(v);
               // we have to remove this vector from the index...
               for (int i = 0; i < v.count(); ++i) {
                  ii = new Integer(i);
                  map = (IPersistentMap)m_kindex.valAt(ii);
                  obj = v.nth(i);
                  // remove this from the edge/node we're looking at from the vertex index
                  tmp = ((IPersistentMap)m_vindex.valAt(obj));
                  if (tmp != null) {
                     tmp = tmp.without(v);
                     if (tmp.count() == 0) tmp = null;
                  }
                  m_vindex.assoc(obj, tmp);
                  // get the map of edges from this middle-map
                  tmp = (IPersistentMap)map.valAt(obj);
                  // remove the edge from this map
                  tmp = tmp.without(v);
                  // if tmp is now empty, we just remove it from the middle-map
                  if (tmp.count() == 0) map = map.without(obj);
                  else map = map.assoc(obj, tmp);
                  // now, if map is empty, we remove it; otherwise, add it in
                  if (map.count() == 0) m_kindex.without(ii);
                  else m_kindex.assoc(ii, map);
               }
            }
         } else if (key instanceof IPersistentSet) {
            IPersistentSet s = (IPersistentSet)key;
            if (m_edges.valAt(s, this) != this) {
               m_edges.without(s);
               map = (IPersistentMap)m_kindex.valAt(null);
               // we have to remove this map from the index...
               for (ISeq q = s.seq(); q != null; q = q.next()) {
                  obj = q.first();
                  // remove this from the edge/node we're looking at from the vertex index
                  tmp = ((IPersistentMap)m_vindex.valAt(obj)).without(s);
                  if (tmp.count() == 0) tmp = null;
                  m_vindex.assoc(obj, tmp);
                  // get the map of edges from this middle-map
                  tmp = (IPersistentMap)map.valAt(obj);
                  // remove the edge from this map
                  tmp = tmp.without(s);
                  // if tmp is now empty, we just remove it from the middle-map
                  if (tmp.count() == 0) map = map.without(obj);
                  else map = map.assoc(obj, tmp);
               }
               // now, if map is empty, we remove it; otherwise, add it in
               if (map.count() == 0) m_kindex.without(null);
               else m_kindex.assoc(null, map);
            }
         } else if (key instanceof IPersistentMap) {
            IPersistentMap m = (IPersistentMap)key;
            if (m_edges.valAt(m, this) != this) {
               MapEntry me;
               Object k;
               m_edges = m_edges.without(m);
               // we have to remove this map from the index...
               for (ISeq q = m.seq(); q != null; q = q.next()) {
                  me = (MapEntry)q.first();
                  k = me.key();
                  obj = me.val();
                  // remove this from the edge/node we're looking at from the vertex index
                  tmp = ((IPersistentMap)m_vindex.valAt(obj)).without(m);
                  if (tmp.count() == 0) tmp = null;
                  m_vindex.assoc(obj, tmp);
                  // now, remove the edges from the edge index
                  map = (IPersistentMap)m_kindex.valAt(k);
                  // get the map of edges from this middle-map
                  tmp = (IPersistentMap)map.valAt(obj);
                  // remove the edge from this map
                  tmp = tmp.without(m);
                  // if tmp is now empty, we just remove it from the middle-map
                  if (tmp.count() == 0) map = map.without(obj);
                  else map = map.assoc(obj, tmp);
                  // now, if map is empty, we remove it; otherwise, add it in
                  if (map.count() == 0) m_kindex.without(k);
                  else m_kindex.assoc(k, map);
               }
            }
         } else {
            // this is a vertex; we have to remove it from the index, which is hard:
            // we must remove all of its edges
            m_vertices.without(key);
            map = (IPersistentMap)m_vindex.valAt(key); // get the list of edges for this vertex
            if (map != null) {
               // go through and clear them out!
               for (ISeq q = map.seq(); q != null; q = q.next())
                  this.without(((MapEntry)q.first()).key());
            }
         }
         return this;
      }
      public final IPersistentMap persistent()
      {
         return new PersistentLabelledHashGraph(m_vertices.persistent(),
                                                m_edges.persistent(),
                                                m_kindex.persistent(),
                                                m_vindex.persistent(),
                                                null);
      }
      public final Object valAt(Object key, Object notFound)
      {
         if (key instanceof IPersistentVector ||
             key instanceof IPersistentSet ||
             key instanceof IPersistentMap)
            return m_edges.valAt(key, notFound);
         else
            return m_vertices.valAt(key, notFound);
      }
      public final Object valAt(Object key)
      {
         return valAt(key, null);
      }
      public final int count()
      {
         return m_vertices.count() + m_edges.count();
      }
      public final ITransientMap conj(Object o)
      {
         if (o instanceof Map.Entry<?,?>) {
            Map.Entry<?,?> me = (Map.Entry<?,?>)o;
            return assoc(me.getKey(), me.getValue());
         } else if (o instanceof IPersistentVector) {
            IPersistentVector v = (IPersistentVector)o;
            if (v.count() != 2)
               throw new IllegalArgumentException("Vector for labelled graph conj must be a pair");
            return assoc(v.nth(0), v.nth(1));
         } else if (o instanceof Seqable) {
            Seqable s = (Seqable)o;
            ITransientMap tr = this;
            for (ISeq q = s.seq(); q != null; q = q.next()) {
               Map.Entry<?,?> me = (Map.Entry<?,?>)q.first();
               tr = tr.assoc(me.getKey(), me.getValue());
            }
            return tr;
         } else
            return this;
      }
      public final Object invoke(Object arg)
      {
         return valAt(arg);
      }
      public final Object invoke(Object arg, Object notFound)
      {
         return valAt(arg, notFound);
      }
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // Static members for use in hash graphs.
   /** EMPTY is a static empty graph, for use in constructing graphs.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public final PersistentLabelledHashGraph EMPTY;
   static
   {
      EMPTY = new PersistentLabelledHashGraph(PersistentHashMap.EMPTY,
                                              PersistentHashMap.EMPTY,
                                              PersistentHashMap.EMPTY,
                                              PersistentHashMap.EMPTY,
                                              null);
   }
   static final long serialVersionUID = 1;


   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The non-static members

   protected final IPersistentMap m_vertices;
   protected final IPersistentMap m_edges;
   protected final IPersistentMap m_kindex;
   protected final IPersistentMap m_vindex;
   protected final IPersistentMap m_meta;
   protected int m_hash;


   /////////////////////////////////////////////////////////////////////////////////////////////////
   // IPersistentGraph methods!
   public IPersistentCollection vertices()
   {
      return m_vertices;
   }
   public IPersistentCollection edges()
   {
      return m_edges;
   }
   public IPersistentCollection match(Object val)
   {
      return (IPersistentCollection)m_vindex.valAt(val);
   }
   // This function searches for edges in the index
   // Args may be a map of wildcard object to IFn (the function must test whether an object
   // is allowed to fill the wildcard).  If args is null, then null is the only tag, and it
   // may be anything.
   public IPersistentCollection match(IPersistentMap args, Object pattern)
   {
      return matchInternal(args, pattern, null, null);
   }
   private IPersistentMap matchInternal(IPersistentMap args, Object pattern, 
                                        HashMap<Object,PersistentMapSorter> uptags, 
                                        Object upindex)
   {
      ISeq q, qq;
      IPersistentMap entry = null;
      Object obj;
      MapEntry me;
      int i, psize = 0;
      IPersistentMap map;
      PersistentMapSorter ms, pms;
      Map.Entry<Object,PersistentMapSorter> meop;
      Iterator<Map.Entry<Object,PersistentMapSorter>> it;
      final int INITIAL_ARG_PQ_SIZE = 4;

      // we need the tags to be a map so that we can get fast lookups and decide what
      // each tag can possibly be
      HashMap<Object,PersistentMapSorter> tags;
      if (args == null || args.count() == 0) {
         tags = new HashMap<Object,PersistentMapSorter>(2);
         tags.put(null, new PersistentMapSorter(INITIAL_ARG_PQ_SIZE));
      } else {
         tags = new HashMap<Object,PersistentMapSorter>(args.count() * 2);
         for (q = args.seq(); q != null; q = q.next())
            tags.put(((MapEntry)q.first()).key(), new PersistentMapSorter(INITIAL_ARG_PQ_SIZE));
      }

      // alright, we need to get the indices we're searching for first and put them into some
      // map-queues for intersecting and unioning
      PersistentMapSorter mapSorter = new PersistentMapSorter(INITIAL_ARG_PQ_SIZE);
      if (pattern instanceof IPersistentVector) {
         IPersistentVector v = (IPersistentVector)pattern;
         psize = v.count();
         Integer ii;
         for (i = 0; i < v.count(); ++i) {
            ii = new Integer(i);
            obj = v.nth(i);
            // see if this is a tag
            ms = tags.get(obj);
            if (ms != null) {
               // we add this map into the tag's map sorter
               ms.add((IPersistentMap)m_kindex.valAt(ii));
            } else {
               // see if there's a sub-match for this specific object in this entry
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap) {
                  entry = this.matchInternal(args, obj, tags, ii);
               } else {
                  // get the entry in the index
                  entry = (IPersistentMap)m_kindex.valAt(ii);
                  // if there is no entry, we know this can't be a match
                  if (entry == null) return null;
                  entry = (IPersistentMap)entry.valAt(obj);
               }
               // if there isn't, this can't be a match
               if (entry == null) return null;
               // add this to the standard mapSorter
               mapSorter.add(entry);
            }
         }
      } else if (pattern instanceof IPersistentSet) {
         // otherwise, iterate through the elements in the set...
         psize = ((IPersistentSet)pattern).count();
         for (q = ((IPersistentSet)pattern).seq(); q != null; q = q.next()) {
            obj = q.first();
            // see if this is a tag
            ms = tags.get(obj);
            if (ms != null) {
               // we add this map into the tag's map sorter
               ms.add((IPersistentMap)m_kindex.valAt(null));
            } else {
               // see if there's a sub-match for this specific object in this entry
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap)
                  entry = this.matchInternal(args, obj, tags, null);
               else {
                  // get the entry in the index
                  entry = (IPersistentMap)m_kindex.valAt(null);
                  // if there are no undirected edges, we can just return nil now
                  if (entry == null) return null;
                  entry = (IPersistentMap)entry.valAt(obj);
               }
               // if there isn't, this can't be a match
               if (entry == null) return null;
               // add this to the standard mapSorter
               mapSorter.add(entry);
            }
         }
      } else if (pattern instanceof IPersistentMap) {
         map = (IPersistentMap)pattern;
         psize = map.count();
         for (q = map.seq(); q != null; q = q.next()) {
            me = (MapEntry)q.first();
            obj = me.val();
            // see if this is a tag
            ms = tags.get(obj);
            if (ms != null) {
               // we add this map into the tag's map sorter
               ms.add((IPersistentMap)m_kindex.valAt(me.key()));
            } else {
               if (obj instanceof IPersistentVector ||
                   obj instanceof IPersistentSet ||
                   obj instanceof IPersistentMap)
                  entry = matchInternal(args, obj, tags, me.key());
               else {
                  // get the entry in the index
                  entry = (IPersistentMap)m_kindex.valAt(me.key());
                  // if there is no entry, we know this can't be a match
                  if (entry == null) return null;
                  // see if there's a sub-match for this specific object in this entry
                  entry = (IPersistentMap)entry.valAt(obj);
               }
               // if there isn't, this can't be a match
               if (entry == null) return null;
               // add this to the standard mapSorter
               mapSorter.add(entry);
            }
         }
      } else {
         // querying on a vertex only
         psize = 0;
         ms = tags.get(pattern);
         if (ms != null) {
            // they are filtering on a function call probably; this needs in insert everything
            for (q = m_kindex.seq(); q != null; q = q.next())
               ms.add((IPersistentMap)((MapEntry)q.first()).val());
         } else {
            // they are just checking if this form exists...
            if (m_vertices.valAt(pattern, this) != this) {
               obj = m_vertices.valAt(pattern);
               return PersistentHashMap.create(pattern, obj);
            } else
               return null;
         }
      }
      // Okay, at this point, we've gathered all the sets into our map sorters!  Intersect the tag
      // maps down into maps of possible nodes mapped to edges then union all the edges in the lower
      // maps; these unioned maps go into mapSorter.
      IFn ifn;
      HashMap<Object,IPersistentMap> upSavedMaps = null;
      if (uptags != null) upSavedMaps = new HashMap<Object,IPersistentMap>(tags.size());
      pms = new PersistentMapSorter(INITIAL_ARG_PQ_SIZE);
      it = tags.entrySet().iterator();
      while (it.hasNext()) {
         meop = it.next();
         ms = meop.getValue();
         // it's possible that this tag was not used in the pattern--see if there was never an 
         // added map
         if (ms.count() == 0) continue;
         // we want the intersection with the sub-maps intersected...
         map = ms.intersectionSubIntersection();
         // from here on, if map ends up empty (ie, nothing fits all criteria), we know there aren't
         // any possible matches to the pattern; currently map contains the mapping of node u to the
         // set of edges that validly assign tag 1 to node u
         if (map == null || map.count() == 0) return null;
         // save the map for later examination if we have uptags
         if (upSavedMaps != null) upSavedMaps.put(meop.getKey(), map);
         // reset our map-sorter...
         pms.reset();
         // okay, map is the set of nodes this arg could be mapped to the edges that match it at
         // all of its occurances; next, see if this object matches the function the user provided
         // note that if we have uptags, we don't check these--this is because they should only be
         // checked once at the very end of the original function call.
         ifn = (args == null || uptags != null? null : (IFn)args.valAt(meop.getKey()));
         for (q = map.seq(); q != null; q = q.next()) {
            me = (MapEntry)q.first();
            if (ifn != null) {
               obj = ifn.invoke(me.key());
               if (obj == null || ((obj instanceof Boolean) && !((Boolean)obj).booleanValue()))
                  continue;
            }
            pms.add((IPersistentMap)me.val());
         }
         map = pms.union();
         // there may not be any matches for this tag...
         if (map.count() == 0) return null;
         // but otherwise, add it to the overall intersector
         mapSorter.add(map);
      }
      // Great; at this point, we just need to get the intersection of all the maps of edges we've collected
      // then filter out the edges that are too big
      map = mapSorter.intersection();
      // we need to dissoc those edges that are the wrong size
      for (q = map.seq(); q != null; q = q.next()) {
         me = (MapEntry)q.first();
         obj = me.key();
         if (obj instanceof IPersistentVector) {
            if (psize != ((IPersistentVector)obj).count())
               map = map.without(obj);
         } else if (obj instanceof IPersistentSet) {
               if (psize != ((IPersistentSet)obj).count())
                  map = map.without(obj);
         } else if (obj instanceof IPersistentMap) {
            if (psize != ((IPersistentMap)obj).count())
               map = map.without(obj);
         } else if (psize != 0) {
            map = map.without(obj);
         }
      }
      if (map.count() == 0) return null;
      // Our final task before returning is to fix the uptags if we were given them
      if (uptags != null) {
         // we need to go through each uptag and intersect each of it's mapped (node, edge-set)
         // pairs, intersect the edge-set with the final return value (map) to find out which edges
         // in edge-set are actually valid assignments for node.  We then update node's entry in the
         // uptags such that every allowable edge e in edge-set is inserted into the uptag's node's 
         // edge-set as the set of edges containing e at edge index upindex.
         IPersistentMap tmpmap;
         Map.Entry<Object,IPersistentMap> meObjMap;
         Iterator<Map.Entry<Object,IPersistentMap>> iit;
         IPersistentMap upIdxMap = (IPersistentMap)m_kindex.valAt(upindex);
         if (upIdxMap == null) return null;
         PersistentMapSorter ppms = new PersistentMapSorter(INITIAL_ARG_PQ_SIZE);
         iit = upSavedMaps.entrySet().iterator();
         while (iit.hasNext()) { // for each tag
            meObjMap = iit.next();
            entry = meObjMap.getValue();
            ms = uptags.get(meObjMap.getKey());
            pms.reset();
            // for each node (mapped to a possible edge-set)
            for (q = entry.seq(); q != null; q = q.next()) {
               me = (MapEntry)q.first();
               obj = me.key(); // this is the node value that is possible for the tag
               ppms.reset();
               for (qq = ((IPersistentMap)me.val()).seq(); qq != null; qq = qq.next()) {
                  me = (MapEntry)qq.first();
                  // see if this is in the final solution set
                  if (map.containsKey(me.key()))
                     // we add the edges in which this edge appears at upindex to the unioner
                     ppms.add((IPersistentMap)upIdxMap.valAt(me.key()));
               }
               // put the union into the new map (we do this by updating entry)
               tmpmap = ppms.union();
               if (tmpmap.count() == 0)
                  entry = entry.without(obj);
               else
                  entry = entry.assoc(obj, tmpmap);
            }
            // at this point, we've completely rebuilt entry into a map for the uptag
            ms.add(entry);
         }
         // additionally, we want to edit map similarly, so that it represents the results
         // positioned at upindex
         ppms.reset();
         for (qq = map.seq(); qq != null; qq = qq.next()) {
            me = (MapEntry)qq.first();
            // lookup the edges that have this edge at upindex
            ppms.add((IPersistentMap)upIdxMap.valAt(me.key()));
         }
         // put the union into the new map (we do this by updating entry)
         map = ppms.union();
         if (map.count() == 0) map = null;
         // that's it!
      }
      // That's it!  we return this map
      return map;
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IPersistentMap members!
   public IPersistentMap assoc(Object key, Object val)
   {
      IPersistentMap V = m_vertices, E = m_edges, vidx = m_vindex, kidx = m_kindex;
      PersistentLabelledHashGraph G;
      IPersistentMap map, tmp;
      Object obj;
      ISeq q;
      if (key instanceof IPersistentVector) {
         E = E.assoc(key, val);
         IPersistentVector v = (IPersistentVector)key;
         Integer ii;
         for (int i = 0; i < v.count(); ++i) {
            ii = new Integer(i);
            obj = v.nth(i);
            // vertex index first
            if (obj instanceof IPersistentVector ||
                obj instanceof IPersistentSet ||
                obj instanceof IPersistentMap) {
               if (E.valAt(obj, this) == this) {
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
               }
            } else {
               if (V.valAt(obj, this) == this) {
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
               }
            }
            tmp = (IPersistentMap)vidx.valAt(obj);
            if (tmp == null) tmp = PersistentHashMap.EMPTY;
            vidx = vidx.assoc(obj, tmp.assoc(key, val));
            // key index next
            map = (IPersistentMap)kidx.valAt(ii);
            if (map == null) {
               map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
            } else {
               tmp = (IPersistentMap)map.valAt(obj);
               if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
               else map = map.assoc(obj, tmp.assoc(key, val));
            }
            kidx = kidx.assoc(ii, map);
         }
      } else if (key instanceof IPersistentSet) {
         E = E.assoc(key, val);
         IPersistentSet s = (IPersistentSet)key;
         map = (IPersistentMap)m_kindex.valAt(null);
         for (q = s.seq(); q != null; q = q.next()) {
            obj = q.first();
            if (obj instanceof IPersistentVector ||
                obj instanceof IPersistentSet ||
                obj instanceof IPersistentMap) {
               if (E.valAt(obj, this) == this) {
                  kidx = kidx.assoc(null, map);
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
                  map = (IPersistentMap)kidx.valAt(null);
               }
            } else {
               if (V.valAt(obj, this) == this) {
                  kidx = kidx.assoc(null, map);
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
                  map = (IPersistentMap)kidx.valAt(null);
               }
            }
            tmp = (IPersistentMap)vidx.valAt(obj);
            if (tmp == null) tmp = PersistentHashMap.EMPTY;
            vidx = vidx.assoc(obj, tmp.assoc(key, val));
            if (map == null)
               map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
            else {
               tmp = (IPersistentMap)map.valAt(obj);
               if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
               else map = map.assoc(obj, tmp.assoc(key, val));
            }
         }
         kidx = kidx.assoc(null, map);
      } else if (key instanceof IPersistentMap) {
         E = E.assoc(key, val);
         IPersistentMap m = (IPersistentMap)key;
         MapEntry me;
         for (q = m.seq(); q != null; q = q.next()) {
            me = (MapEntry)q.first();
            obj = me.val();
            if (obj instanceof IPersistentVector ||
                obj instanceof IPersistentSet ||
                obj instanceof IPersistentMap) {
               if (E.valAt(obj, this) == this) {
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
               }
            } else {
               if (V.valAt(obj, this) == this) {
                  G = new PersistentLabelledHashGraph(V, E, kidx, vidx, null);
                  G = (PersistentLabelledHashGraph)G.assoc(obj, null);
                  V = G.m_vertices;
                  E = G.m_edges;
                  kidx = G.m_kindex;
                  vidx = G.m_vindex;
               }
            }
            tmp = (IPersistentMap)vidx.valAt(obj);
            if (tmp == null) tmp = PersistentHashMap.EMPTY;
            vidx = vidx.assoc(obj, tmp.assoc(key, val));
            // next, key index
            map = (IPersistentMap)kidx.valAt(me.key());
            if (map == null)
               map = PersistentHashMap.create(obj, PersistentHashMap.create(key, val));
            else {
               tmp = (IPersistentMap)map.valAt(obj);
               if (tmp == null) map = map.assoc(obj, PersistentHashMap.create(key, val));
               else map = map.assoc(obj, tmp.assoc(key, val));
            }
            kidx = kidx.assoc(me.key(), map);
         }
      } else {
         if (V.valAt(key, this) == this)
            vidx = vidx.assoc(key, null);
         V = V.assoc(key, val);
      }
      return new PersistentLabelledHashGraph(V, E, kidx, vidx, m_meta);
   }
   public IPersistentMap assocEx(Object key, Object val)
   {
      if (this.valAt(key, this) != this)
         throw Util.runtimeException("Key already present");
      return assoc(key, val);
   }
   public IPersistentMap without(Object key)
   {
      IPersistentMap V = m_vertices, E = m_edges, vidx = m_vindex, kidx = m_kindex;
      IPersistentMap map, tmp;
      Object obj;
      ISeq q;
      if (key instanceof IPersistentVector) {
         IPersistentVector v = (IPersistentVector)key;
         if (E.valAt(v, this) != this) {
            Integer ii;
            E = E.without(v);
            // we have to remove this vector from the index...
            for (int i = 0; i < v.count(); ++i) {
               ii = new Integer(i);
               map = (IPersistentMap)kidx.valAt(ii);
               obj = v.nth(i);
               // remove this from the edge/node we're looking at from the vertex index
               tmp = ((IPersistentMap)vidx.valAt(obj)).without(v);
               if (tmp.count() == 0) tmp = null;
               vidx = vidx.assoc(obj, tmp);
               // get the map of edges from this middle-map
               tmp = (IPersistentMap)map.valAt(obj);
               // remove the edge from this map
               tmp = tmp.without(v);
               // if tmp is now empty, we just remove it from the middle-map
               if (tmp.count() == 0) map = map.without(obj);
               else map = map.assoc(obj, tmp);
               // now, if map is empty, we remove it; otherwise, add it in
               if (map.count() == 0) kidx = kidx.without(ii);
               else kidx = kidx.assoc(ii, map);
            }
         }
      } else if (key instanceof IPersistentSet) {
         IPersistentSet s = (IPersistentSet)key;
         if (E.valAt(s, this) != this) {
            E = E.without(s);
            map = (IPersistentMap)kidx.valAt(null);
            // we have to remove this map from the index...
            for (q = s.seq(); q != null; q = q.next()) {
               obj = q.first();
               // remove this from the edge/node we're looking at from the vertex index
               tmp = ((IPersistentMap)vidx.valAt(obj)).without(s);
               if (tmp.count() == 0) tmp = null;
               vidx = vidx.assoc(obj, tmp);
               // get the map of edges from this middle-map
               tmp = (IPersistentMap)map.valAt(obj);
               // remove the edge from this map
               tmp = tmp.without(s);
               // if tmp is now empty, we just remove it from the middle-map
               if (tmp.count() == 0) map = map.without(obj);
               else map = map.assoc(obj, tmp);
            }
            // now, if map is empty, we remove it; otherwise, add it in
            if (map.count() == 0) kidx = kidx.without(null);
            else kidx = kidx.assoc(null, map);
         }
      } else if (key instanceof IPersistentMap) {
         IPersistentMap m = (IPersistentMap)key;
         if (E.valAt(m, this) != this) {
            MapEntry me;
            Object k;
            E = E.without(m);
            // we have to remove this map from the index...
            for (q = m.seq(); q != null; q = q.next()) {
               me = (MapEntry)q.first();
               k = me.key();
               obj = me.val();
               // remove this from the edge/node we're looking at from the vertex index
               tmp = ((IPersistentMap)vidx.valAt(obj)).without(m);
               if (tmp.count() == 0) tmp = null;
               vidx = vidx.assoc(obj, tmp);
               // now, remove the edges from the edge index
               map = (IPersistentMap)kidx.valAt(k);
               // get the map of edges from this middle-map
               tmp = (IPersistentMap)map.valAt(obj);
               // remove the edge from this map
               tmp = tmp.without(m);
               // if tmp is now empty, we just remove it from the middle-map
               if (tmp.count() == 0) map = map.without(obj);
               else map = map.assoc(obj, tmp);
               // now, if map is empty, we remove it; otherwise, add it in
               if (map.count() == 0) kidx = kidx.without(k);
               else kidx = kidx.assoc(k, map);
            }
         }
      } else {
         PersistentLabelledHashGraph G;
         // this is a vertex; we have to remove it from the index, which is hard:
         // we must remove all of its edges
         map = (IPersistentMap)m_vindex.valAt(key); // get the list of edges for this vertex
         if (map != null) {
            ITransientMap tr = ((ITransientMap)asTransient()).without(key);
            G = (PersistentLabelledHashGraph)((IObj)tr.persistent()).withMeta(m_meta);
         } else
            G = new PersistentLabelledHashGraph(m_vertices.without(key), m_edges, 
                                                m_kindex, m_vindex, m_meta);
         return G;
      }
      return new PersistentLabelledHashGraph(V, E, kidx, vidx, m_meta);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Iterable methods!
   public Iterator<?> iterator()
   {
      return new SeqIterator(seq());
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IFn methods!
   public Object invoke(Object arg1) 
   {
      return valAt(arg1);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IHashEq methods!
   //public int hasheq()
   //{
   //   int hash = 0;
   //   Object e;
   //   for(ISeq s = seq(); s != null; s = s.next()) {
   //      e = s.first();
   //      hash +=  Util.hasheq(e);
   //   }
   //   return hash;
   //}

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Associative methods!
   public boolean containsKey(Object key)
   {
      if (key instanceof IPersistentVector ||
          key instanceof IPersistentSet ||
          key instanceof IPersistentMap)
         return m_edges.containsKey(key);
      else 
         return m_vertices.containsKey(key);
   }
   public IMapEntry entryAt(Object key)
   {
      if (key instanceof IPersistentVector ||
          key instanceof IPersistentSet ||
          key instanceof IPersistentMap)
         return m_edges.entryAt(key);
      else 
         return m_vertices.entryAt(key);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IPersistentCollection methods!
   public IPersistentCollection cons(Object o)
   {
      if (o instanceof Map.Entry<?,?>) {
         Map.Entry<?,?> e = (Map.Entry<?,?>) o;
         return assoc(e.getKey(), e.getValue());
      } else if (o instanceof IPersistentVector) {
         IPersistentVector v = (IPersistentVector) o;
         if (v.count() != 2)
            throw new IllegalArgumentException("Vector arg to graph conj must be a pair");
         return assoc(v.nth(0), v.nth(1));
      }
      IPersistentMap ret = this;
      for (ISeq es = RT.seq(o); es != null; es = es.next()) {
         Map.Entry<?,?> e = (Map.Entry<?,?>) es.first();
         ret = ret.assoc(e.getKey(), e.getValue());
      }
      return ret;
   }
   public int count()
   {
      return m_vertices.count() + m_edges.count();
   }
   public IPersistentCollection empty()
   {
      return EMPTY;
   }
   public boolean equiv(Object obj)
   {
      if (!(obj instanceof Map<?,?>))
         return false;
      if (obj instanceof IPersistentMap && !(obj instanceof MapEquivalence))
         return false;
      Map<?,?> m = (Map<?,?>) obj;
      if (m.size() != count())
         return false;
      boolean found;
      for (ISeq s = m_vertices.seq(); s != null; s = s.next()) {
         Map.Entry<?,?> e = (Map.Entry<?,?>) s.first();
         found = m.containsKey(e.getKey());
         if(!found || !Util.equiv(e.getValue(), m.get(e.getKey())))
            return false;
      }
      for (ISeq s = m_edges.seq(); s != null; s = s.next()) {
         Map.Entry<?,?> e = (Map.Entry<?,?>) s.first();
         found = m.containsKey(e.getKey());
         if(!found || !Util.equiv(e.getValue(), m.get(e.getKey())))
            return false;
      }
      return true;
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Seqable methods!
   public ISeq seq()
   {
      return HashGraphSeq.create(m_vertices.seq(), m_edges.seq(), null);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The ILookup methods!
   public Object valAt(Object key)
   {
      if (key instanceof IPersistentCollection)
         return m_edges.valAt(key);
      else
         return m_vertices.valAt(key);
   }
   public Object valAt(Object key, Object notFound)
   {
      if (key instanceof IPersistentVector ||
          key instanceof IPersistentSet ||
          key instanceof IPersistentMap)
         return m_edges.valAt(key, notFound);
      else
         return m_vertices.valAt(key, notFound);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IObj methods!
   public IObj withMeta(IPersistentMap meta)
   {
      return new PersistentLabelledHashGraph(m_vertices, m_edges, m_kindex, m_vindex, meta);
   }
   public IPersistentMap meta()
   {
      return m_meta;
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IEditableCollection members!
   public ITransientCollection asTransient()
   {
      return new TransientLabelledHashGraph(this);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // Object functions!
   public int hashCode()
   {
      if (m_hash == -1)
         m_hash = m_vindex.hashCode() * m_vertices.hashCode() + m_kindex.hashCode() * m_edges.hashCode();
      return m_hash;
   }
   public boolean equals(Object obj)
   {
      return APersistentMap.mapEquals(this, obj);
   }
   public String toString()
   {
      return "#Graph{" + m_vertices.toString() + ", " + m_edges.toString() + "}";
   }


   /////////////////////////////////////////////////////////////////////////////////////////////////
   // And finally, the constructor and creation utilities!

   /** PersistentLabelledHashGraph can be constructed with a map of vertices, a map of edges, a key-
    *  index map, a node-index map, and metadata.  If these are not properly formatted, then 
    *  behavior is undefined; it is suggested that graph creation be done using the create() static
    *  functions rather than the constructor.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   protected PersistentLabelledHashGraph(IPersistentMap V, IPersistentMap E, 
                                         IPersistentMap kindex, IPersistentMap vindex,
                                         IPersistentMap meta)
   {
      m_vertices = (V == null? PersistentHashMap.EMPTY : V);
      m_edges = (E == null? PersistentHashMap.EMPTY : E);
      m_kindex = (kindex == null? PersistentHashMap.EMPTY : kindex);
      m_vindex = (vindex == null? PersistentHashMap.EMPTY : vindex);
      m_meta = meta;
   }
   /** Creates a persistent labelled hash graph using the (key value) pairs of Map m as vertices and
    *  edges (keys) and labels (values) of the new graph.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(Map<?,?> m)
   {
      if (m == null) return EMPTY;
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      for (Map.Entry<?,?> me : m.entrySet())
         G.assoc(me.getKey(), me.getValue());
      return (IPersistentGraph)G.persistent();
   }
   /** Creates a persistent labelled hash graph using the (key value) pairs of Map m as vertices and
    *  edges (keys) and labels (values) of the new graph, and assigning the new graph's metadata to 
    *  be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(IPersistentMap meta, Map<?,?> m)
   {
      return (IPersistentGraph)((IObj)create(m)).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the seq q provided.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(ISeq q)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      for (; q != null; q = q.next().next()) {
         if (q.next() == null)
            throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                             q.first()));
         G.assoc(q.first(), q.next().first());
      }
      return (IPersistentGraph)G.persistent();
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the seq q provided; 
    *  throws an exception if an a key occurs twice in the seq.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(ISeq q)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      for (; q != null; q = q.next().next()) {
         if (q.next() == null)
            throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                             q.first()));
         if (G.valAt(q.first(), G) != G)
            throw new IllegalArgumentException(String.format("Duplicate key: %s", q.first()));
         G.assoc(q.first(), q.next().first());
      }
      return (IPersistentGraph)G.persistent();
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the seq q provided; sets
    *  the new graph's metadata to the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(IPersistentMap meta, ISeq q)
   {
      return (IPersistentGraph)((IObj)create(q)).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the seq q provided; 
    *  throws an exception if an a key occurs twice in the seq.  Assigns the new graph's meta-data
    *  to be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(IPersistentMap meta, ISeq q)
   {
      return (IPersistentGraph)((IObj)createWithCheck(q)).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the object list provided.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(Object... init)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      if (init.length % 2 == 1)
         throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                          init[init.length - 1]));
      for (int i = 0; i < init.length; i += 2)
         G.assoc(init[i], init[i+1]);
      return (IPersistentGraph)G.persistent();
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the object list provided; 
    *  throws an exception if an a key occurs twice in the seq.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(Object... init)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      if (init.length % 2 == 1)
         throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                          init[init.length - 1]));
      for (int i = 0; i < init.length; i += 2) {
         if (G.valAt(init[i], G) != G)
            throw new IllegalArgumentException(String.format("Duplicate key: %s", init[i]));
         G.assoc(init[i], init[i+1]);
      }
      return (IPersistentGraph)G.persistent();
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the object list provided;
    *  assigns the new graph's meta-data to be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(IPersistentMap meta, Object... init)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      if (init.length % 2 == 1)
         throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                          init[init.length - 1]));
      for (int i = 0; i < init.length; i += 2)
         G.assoc(init[i], init[i+1]);
      return (IPersistentGraph)((IObj)G.persistent()).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph from the pairs of items in the object list provided; 
    *  throws an exception if an a key occurs twice in the seq.  Assigns the new graph's meta-data
    *  to be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(IPersistentMap meta, Object... init)
   {
      ITransientMap G = (ITransientMap)EMPTY.asTransient();
      if (init.length % 2 == 1)
         throw new IllegalArgumentException(String.format("No value specified for key: %s",
                                                          init[init.length - 1]));
      for (int i = 0; i < init.length; i += 2) {
         if (G.valAt(init[i], G) != G)
            throw new IllegalArgumentException(String.format("Duplicate key: %s", init[i]));
         G.assoc(init[i], init[i+1]);
      }
      return (IPersistentGraph)((IObj)G.persistent()).withMeta(meta);
   }
}

