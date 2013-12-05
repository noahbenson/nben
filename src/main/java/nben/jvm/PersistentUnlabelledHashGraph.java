////////////////////////////////////////////////////////////////////////////////////////////////////
// PersistentUnlabelledHashGraph.java, part of nben, a mathematics library for clojure.
// This file defines the persistent unlabelled graph implementation using clojure's persistent 
// hash maps.
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
import clojure.lang.*;
import java.util.*;
import java.io.Serializable;
import java.lang.reflect.Array;

////////////////////////////////////////////////////////////////////////////////////////////////////
// NOTES
// This is just a wrapper for the labelled version of the graph in exactly the same way that set is
// a wrapper for map.

/** PersistentUnabelledHashGraph is an implementation of the IPersistentGraph interface.  The class
 *  can be treated as a persistent graph but also acts identically to a PersistentHashSet for all
 *  intents and purposes; adding vertices and edges is done exactly as if you were conj'ing them
 *  into a set, for example.  
 * 
 *  @author Noah C. Benson
 *  @version 1.0
 */
@SuppressWarnings("unchecked") // we supress warnings for not using generic Set and Collection's.
public class PersistentUnlabelledHashGraph
   extends AFn
   implements IPersistentGraph, IObj, IEditableCollection, IPersistentSet, Collection,
              Set, Serializable //, IHashEq
{
   /////////////////////////////////////////////////////////////////////////////////////////////////
   // Because clojure's PersistentHashSet doesn't allow us to create a set from the keys of a hash
   // (even though that's how it stores its data internally), we have to duplicate the whole
   // class but with useful constructors here:
   protected static class SetFromKeys extends APersistentSet implements IObj, IEditableCollection
   {
      static public final SetFromKeys EMPTY = new SetFromKeys(PersistentHashMap.EMPTY);
      final IPersistentMap _meta;
      final IPersistentMap impl;
      final static long serialVersionUID = 1;
      public SetFromKeys(IPersistentMap meta, IPersistentMap impl)
      {
         super(impl);
         this.impl = (impl == null? PersistentHashMap.EMPTY : impl);
         this._meta = meta;
      }
      public SetFromKeys(IPersistentMap impl)
      {
         super(impl);
         this.impl = (impl == null? PersistentHashMap.EMPTY : impl);
         this._meta = null;
      }
      public IPersistentSet disjoin(Object key)
      {
         if(contains(key))
            return new SetFromKeys(meta(), impl.without(key));
         return this;
      }
      public IPersistentSet cons(Object o)
      {
         if(contains(o))
            return this;
         return new SetFromKeys(meta(), impl.assoc(o,o));
      }
      public IPersistentCollection empty()
      {
         return EMPTY.withMeta(meta());
      }
      public SetFromKeys withMeta(IPersistentMap meta)
      {
         return new SetFromKeys(meta, impl);
      }
      public ITransientCollection asTransient()
      {
         return new TransientSetFromKeys(((PersistentHashMap)impl).asTransient());
      }
      public IPersistentMap meta()
      {
         return _meta;
      }
      // and since the ATransientSet constructor is private, we have to reproduce that whole class
      // here (and down below) as well!
      static final class TransientSetFromKeys extends AFn implements ITransientSet
      {
         ITransientMap m_impl;
         TransientSetFromKeys(ITransientMap impl)
         {
            m_impl = impl;
         }
         public int count()
         {
            return m_impl.count();
         }
         public ITransientSet conj(Object val)
         {
            ITransientMap m = m_impl.assoc(val, val);
            if (m != m_impl) this.m_impl = m;
            return this;
         }
         public boolean contains(Object key)
         {
            return this != m_impl.valAt(key, this);
         }
         
         public ITransientSet disjoin(Object key)
         {
            ITransientMap m = m_impl.without(key);
            if (m != m_impl) this.m_impl = m;
            return this;
         }
         
         public Object get(Object key)
         {
            return m_impl.valAt(key);
         }
         
         public Object invoke(Object key, Object notFound)
         {
            return m_impl.valAt(key, notFound);
         }
         
         public Object invoke(Object key)
         {
            return m_impl.valAt(key);
         }
         public IPersistentCollection persistent()
         {
            return new SetFromKeys(null, m_impl.persistent());
         }
      }
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The TransientUnlabelledHashGraph class!
   protected static final class TransientUnlabelledHashGraph extends AFn implements ITransientSet
   {
      ITransientMap m_impl;
      TransientUnlabelledHashGraph(ITransientMap tr)
      {
         m_impl = tr;
      }
      public int count()
      {
         return m_impl.count();
      }
      public ITransientSet conj(Object val)
      {
         ITransientMap m = m_impl.assoc(val, val);
         if (m != m_impl) this.m_impl = m;
         return this;
      }
      public boolean contains(Object key)
      {
         return this != m_impl.valAt(key, this);
      }

      public ITransientSet disjoin(Object key)
      {
         ITransientMap m = m_impl.without(key);
         if (m != m_impl) this.m_impl = m;
         return this;
      }

      public Object get(Object key)
      {
         return m_impl.valAt(key);
      }

      public Object invoke(Object key, Object notFound)
      {
         return m_impl.valAt(key, notFound);
      }

      public Object invoke(Object key)
      {
         return m_impl.valAt(key);
      }
      public IPersistentCollection persistent()
      {
         return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)m_impl.persistent());
      }
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // Static members for use in hash graphs.
   /** EMPTY is a static empty graph, for use in constructing graphs.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public final PersistentUnlabelledHashGraph EMPTY;
   static
   {
      EMPTY = new PersistentUnlabelledHashGraph(PersistentLabelledHashGraph.EMPTY);
   }
   static final long serialVersionUID = 1;

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The non-static members

   protected final PersistentLabelledHashGraph m_impl;

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // IPersistentGraph methods!
   public IPersistentCollection vertices()
   {
      return new SetFromKeys(null, (IPersistentMap)m_impl.vertices());
   }
   public IPersistentCollection edges()
   {
      return new SetFromKeys(null, (IPersistentMap)m_impl.edges());
   }
   public IPersistentCollection match(Object val)
   {
      IPersistentMap tmp = (IPersistentMap)m_impl.match(val);
      if (tmp == null) return null;
      else return new SetFromKeys(null, tmp);
   }
   public IPersistentCollection match(IPersistentMap args, Object pattern)
   {
      IPersistentMap tmp = (IPersistentMap)m_impl.match(args, pattern);
      if (tmp == null) return null;
      else return new SetFromKeys(null, tmp);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IPersistentSet members!
   public boolean contains(Object key)
   {
      return this != m_impl.valAt(key, this);
   }
   public IPersistentSet conj(Object val)
   {
      if (contains(val)) return this;
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)m_impl.assoc(val, val));
   }
   public IPersistentSet disjoin(Object val)
   {
      if (!contains(val)) return this;
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)m_impl.without(val));
   }
   public Object get(Object val)
   {
      if (m_impl.valAt(val, this) == this) return null;
      else return val;
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Set methods!
   public boolean add(Object o)
   {
      throw new UnsupportedOperationException();
   }

   public boolean remove(Object o)
   {
      throw new UnsupportedOperationException();
   }

   public boolean addAll(Collection c)
   {
      throw new UnsupportedOperationException();
   }

   public void clear()
   {
      throw new UnsupportedOperationException();
   }

   public boolean retainAll(Collection c)
   {
      throw new UnsupportedOperationException();
   }

   public boolean removeAll(Collection c)
   {
      throw new UnsupportedOperationException();
   }

   public boolean containsAll(Collection c)
   {
      for (Object o : c) {
         if (!contains(o))
            return false;
      }
      return true;
   }
   public Object[] toArray()
   {
      return RT.seqToArray(seq());
   }
   public Object[] toArray(Object[] a)
   {
      int len = count(), i;
      Object[] dest = (Object[])Array.newInstance(a.getClass(), len);
      ISeq q;
      for (q = seq(), i = 0; q != null; ++i, q = q.next())
         dest[i] = q.first();
      if (a.length < len)
         // Make a new array of a's runtime type, but my contents:
         return dest;
      System.arraycopy(dest, 0, a, 0, len);
      if (a.length > len)
         a[len] = null;
      return a;
   }
   public int size()
   {
      return count();
   }
   public boolean isEmpty()
   {
      return count() == 0;
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
   // The IFn methods!
   public Object invoke(Object arg1) 
   {
      return get(arg1);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Iterable methods!
   public Iterator iterator()
   {
      return (new SetFromKeys(null, m_impl)).iterator();
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IPersistentCollection methods!
   public IPersistentCollection cons(Object o)
   {
      return conj(o);
   }
   public int count()
   {
      return m_impl.count();
   }
   public IPersistentCollection empty()
   {
      return EMPTY;
   }
   public boolean equiv(Object o)
   {
      return (new SetFromKeys(null, m_impl)).equiv(o);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The Seqable methods!
   public ISeq seq()
   {
      return RT.keys(m_impl);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IObj methods!
   public IObj withMeta(IPersistentMap meta)
   {
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)m_impl.withMeta(meta));
   }
   public IPersistentMap meta()
   {
      return m_impl.meta();
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // The IEditableCollection members!
   public ITransientCollection asTransient()
   {
      return new TransientUnlabelledHashGraph((ITransientMap)m_impl.asTransient());
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // Object functions!
   public int hashCode()
   {
      return m_impl.hashCode();
   }
   public boolean equals(Object obj)
   {
      return equiv(obj);
   }

   /////////////////////////////////////////////////////////////////////////////////////////////////
   // And finally, the constructor and creation utilities!
   /** PersistentUnlabelledHashGraph can be constructed from a PersistentLabelledHashGraph, whose 
    *  labels are dropped; it is generally suggested that the create() functions be used when 
    *  creating an unlabelled hash graph.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   public PersistentUnlabelledHashGraph(PersistentLabelledHashGraph impl)
   {
      if (impl == null) m_impl = PersistentLabelledHashGraph.EMPTY;
      else m_impl = impl;
   }
   /** Creates a persistent labelled hash graph using the items in the seq q m as vertices and
    *  edges.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(ISeq q)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (; q != null; q = q.next())
         G.assoc(q.first(), q.first());
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent());
   }
   /** Creates a persistent labelled hash graph using the items in the seq q m as vertices and
    *  edges.  Throws an exception if an item occurs twice in q.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(ISeq q)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (; q != null; q = q.next()) {
         if (G.valAt(q.first(), G) != G)
            throw new IllegalArgumentException(String.format("Duplicate object: %s", q.first()));
         G.assoc(q.first(), q.first());
      }
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent());
   }
   /** Creates a persistent labelled hash graph using the items in the seq q m as vertices and
    *  edges.  Assigns the new graph's meta-data to be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(IPersistentMap meta, ISeq q)
   {
      return (IPersistentGraph)((PersistentUnlabelledHashGraph)create(q)).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph using the items in the seq q m as vertices and
    *  edges.  Throws an exception if an item occurs twice in q.  Assigns the new graph's meta-data
    *  to be the map meta.
    *
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(IPersistentMap meta, ISeq q)
   {
      return (IPersistentGraph)((PersistentUnlabelledHashGraph)createWithCheck(q)).withMeta(meta);
   }
   /** Creates a persistent labelled hash graph using the objects passed as vertices and edges.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(Object... init)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (int i = 0; i < init.length; ++i)
         G.assoc(init[i], init[i]);
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent());
   }
   /** Creates a persistent labelled hash graph using the objects passed as vertices and edges.
    *  Throws an exception if an object occurs twice in the list.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(Object... init)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (int i = 0; i < init.length; ++i) {
         if (G.valAt(init[i], G) != G)
            throw new IllegalArgumentException(String.format("Duplicate object: %s", init[i]));
         G.assoc(init[i], init[i]);
      }
      return new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent());
   }
   /** Creates a persistent labelled hash graph using the objects passed as vertices and edges.
    *  Assigns the new graph's meta-data to be the map meta.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph create(IPersistentMap meta, Object... init)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (int i = 0; i < init.length; ++i)
         G.assoc(init[i], init[i]);
      return (IPersistentGraph)
         (new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent()))
         .withMeta(meta);
   }
   /** Creates a persistent labelled hash graph using the objects passed as vertices and edges.
    *  Assigns the new graph the meta-data map passed as meta.  Throws an exception if an object 
    *  occurs twice in the list.
    * 
    *  @author Noah C. Benson
    *  @version 1.0
    */
   static public IPersistentGraph createWithCheck(IPersistentMap meta, Object... init)
   {
      ITransientMap G = (ITransientMap)PersistentLabelledHashGraph.EMPTY.asTransient();
      for (int i = 0; i < init.length; ++i) {
         if (G.valAt(init[i], G) != G)
            throw new IllegalArgumentException(String.format("Duplicate object: %s", init[i]));
         G.assoc(init[i], init[i]);
      }
      return (IPersistentGraph)
         (new PersistentUnlabelledHashGraph((PersistentLabelledHashGraph)G.persistent()))
         .withMeta(meta);
   }
}

