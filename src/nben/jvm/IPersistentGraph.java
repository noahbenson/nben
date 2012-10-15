////////////////////////////////////////////////////////////////////////////////////////////////////
// IPersistentGraph.java, part of nben, a mathematics library for clojure.
// This file defines the persistent graph interface.
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
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;

/** IPersistentGraph is an interface for a persistent graph.  It is highly minimal and is intended
 *  that any implementing class will extended either IPersistentMap (for labelled graphs) or 
 *  IPersistentSet (for unlabelled graphs).
 * 
 *  @author Noah C. Benson
 *  @version 1.0
 */
public interface IPersistentGraph extends IPersistentCollection
{
   // We must be able to get a list of edges or vertices (generally a set or map)
   /** The edges() function must return a collection of the edges in the graph.
    *
    *  @author Noah C. Benson
    *  @version 1.0
    */
   public IPersistentCollection edges();
   /** The vertices() function must return a collection of the vertices in the graph.
    *
    *  @author Noah C. Benson
    *  @version 1.0
    */
   public IPersistentCollection vertices();

   /** The match(wildcards, pattern) function must be able to query the graph for any edges or 
    *  vertices contained within.  The wildcards must be a map of wildcards (e.g., :1) to a fn that
    *  returns true for any object in the graph that is allowed to match that wildcard (or nil for
    *  no restrictions).  If wildcards is nil, then nil {nil nil} is used as the wildcard.  The 
    *  pattern is matched exactly with substitutions for wildcards allowed; all possible matches to
    *  the given pattern are returned as a set (unlabelled graphs) or a map (labelled graphs).
    *
    *  @author Noah C. Benson
    *  @version 1.0
    */
   public IPersistentCollection match(IPersistentMap wildcards, Object pattern);
   /** The match(containingThis) function returns a set of all edges containing the object
    *  containingThis at any point.  A query of G.match(x) will return [x y] but not 
    *  [[x x] y] because the edge [[x x] y] contains [x x] and y but not x.
    *
    *  @author Noah C. Benson
    *  @version 1.0
    */
   public IPersistentCollection match(Object containingThis);
}
