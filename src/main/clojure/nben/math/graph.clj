;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graph.clj, part of nben, a mathematics library for clojure.
;; This file defines the clojure interface for interacting with nben's persistent graph classes.
;; 
;; Copyright (C) 2012 Noah C. Benson
;;
;; This file is part of the nben clojure library.
;;
;; The nben clojure library is free software: you can redistribute it and/or modify it under the 
;; terms of the GNU General Public License as published by the Free Software Foundation, either 
;; version 3 of the License, or (at your option) any later version.
;;
;; The nben clojure library is distributed in the hope that it will be useful, but WITHOUT ANY 
;; WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with the nben clojure
;; library.  If not, see <http:;;www.gnu.org/licenses/>.
;;

(ns nben.math.graph
  (:import nben.jvm.IPersistentGraph nben.jvm.PersistentLabelledHashGraph
           nben.jvm.PersistentUnlabelledHashGraph)
  (:use clojure.set))

;; just for internal error use
(defn- argerr [txt] (throw (IllegalArgumentException. txt)))

(defn graph?
  {:tag Boolean
   :doc "True if the object given is an IPersistentGraph; false otherwise"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [obj] (instance? IPersistentGraph obj))

(defn labelled?
  {:tag Boolean
   :doc "True if the object given is a graph and has labels; false otherwise"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [obj] (if (instance? PersistentLabelledHashGraph obj) true false))
(defn unlabelled?
  {:tag Boolean
   :doc "True if the object given is a graph and does not have labels; false otherwise"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [obj] (if (instance? PersistentUnlabelledHashGraph obj) true false))

(defn labelled-graph
  {:tag nben.jvm.IPersistentGraph
   :doc "Creates and returns a labelled IPersistentGraph from the key/value pairs provided"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [& keyvals] (nben.jvm.PersistentLabelledHashGraph/create keyvals))
(defn unlabelled-graph
  {:tag nben.jvm.IPersistentGraph
   :doc "Creates and returns an unlabelled IPersistentGraph from the objects provided"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [& more] (nben.jvm.PersistentUnlabelledHashGraph/create more))

(defn vertices
  {:doc (str "Returns a persistent collection of all vertices in the graph; labelled graphs return"
             " a map of vertex to label while unlabelled graphs return a set of vertices")
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [^IPersistentGraph G] (.vertices G))
(defn edges
  {:doc (str "Returns a persistent collection of all edges in the graph; labelled graphs return"
             " a map of edge to label while unlabelled graphs return a set of edges.  If an object"
             " is passed as a second argument, only edges containing that object are returned")
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  ([^IPersistentGraph G] (.edges G))
  ([^IPersistentGraph G obj] (.match G obj)))

(defn neighbors
  {:doc "Returns the directed or undirected neighbors of the node"
   :tag clojure.lang.IPersistentCollection
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [^IPersistentGraph G u &{:keys [forward      reverse       undirected]
                           :or   {forward true reverse false undirected true}}]
  (if (labelled? G)
    (loop [tr (transient {})
           s (seq (merge (if undirected (.match G nil #{u nil}) nil)
                         (if forward (.match G nil [u nil]) nil)
                         (if reverse (.match G nil [nil u]) nil)))]
      (cond (nil? s) (persistent! tr)
            (= u (first (key (first s))))
            (recur (assoc! tr (second (key (first s))) (val (first s))) (next s))
            :else
            (recur (assoc! tr (first (key (first s))) (val (first s))) (next s))))
    (loop [tr (transient #{})
           s (seq (union (if undirected (.match G nil #{u nil}) nil)
                         (if forward (.match G nil [u nil]) nil)
                         (if reverse (.match G nil [nil u]) nil)))]
      (cond (nil? s) (persistent! tr)
            (= u (first (first s)))
            (recur (conj! tr (fnext (first s))) (next s))
            :else
            (recur (conj! tr (first (first s))) (next s))))))

(defn search
  {:tag clojure.lang.IPersistentCollection
   :doc (str "Queries a graph for elements matching pattern using the given args; the arguments may"
             " be nil (in which case nil is the only wildcard) or they may be a map of object (the"
             " chosen wildcard) to a function, which should return true for any object that is"
             " allowed to match that wildcard.  A function can be nil if any match is allowed."
             " If no args parameter is given, then it defaults to {:1 nil :2 nil :3 nil :4 nil :5"
             " nil}.")
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  ([^IPersistentGraph G ^clojure.lang.IPersistentMap args pattern] (.match G args pattern))
  ([^IPersistentGraph G pattern] (.match G {:1 nil :2 nil :3 nil :4 nil :5 nil} pattern)))
  
  

;; The various graph searches
(defn dfs
  {:tag clojure.lang.Seqable
   :doc "Yields a lazy seq of the vertices in the graph in depth-first-search order"
   :static true
   :author "Noah C. Benson"
   :added "1.0"}
  [^IPersistentGraph G &{:keys [start reachable visit]
                         :or {start nil
                              reachable (if (labelled? G)
                                          (fn [u] (keys (neighbors G u)))
                                          (fn [u] (neighbors G u)))
                              visit (fn [u] u)}}]
  (letfn [(step [stack marked]
                  (loop [s (seq stack)]
                    (cond (nil? s) nil
                          (contains? marked (first s)) (recur (next s))
                          (contains? G (first s))
                          (lazy-seq
                           (cons (visit (first s))
                                 (step (loop [st (next s) q (seq (reachable (first s)))]
                                         (if (nil? q) st (recur (cons (first q) st) (next q))))
                                       (conj marked (first s)))))
                          :else
                          (argerr "Given vertex/edge is not in the graph"))))]
    (step (if (nil? start)
            (if (labelled? G) (keys (.vertices G)) (.vertices G))
            start)
          #{})))



        
    