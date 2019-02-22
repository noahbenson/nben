;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data.clj, part of nben, a mathematics library for the JVM.
;; This namespace adds types for managing lazy maps and nested collections.
;; 
;; Copyright (C) 2016 Noah C. Benson
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

(ns nben.util.test
  (:use nben.util)
  (:use nben.util.data)
  (:use clojure.test))

(def map-database
  #{{:eid 1, :type :tree, :data {:species :oak, :position [0 0 1], :props #{:old :strong}}
     :insts [10 13 15 88]}
    {:eid 2, :type :tree, :data {:species :maple, :position [1 1 2], :props :young}
     :insts [3 87 65 55]}
    {:eid 2, :type :tree, :data {:species :maple, :position [0 2 2], :props #{:old :tall}}
     :insts [2 74 30 31]}
    {:eid 3, :type :rock, :data {:type :metamorphic, :name :marble, :position [1 1 1]}
     :insts [77 54 20 5]}
    {:eid 4, :type :rock, :data {:type :sedimentary, :name :shale, :position [0 0 0]}
     :insts [1 9 64 12]}})
(def par-database (->> map-database (map ref) set ref))

(def q-old-trees
  (pattern {:data {:species :s, :position [:x :y :z], :props :old}} [s x y z] true :allow :any))

(deftest test-sats
  (let [s (sats q-old-trees map-database)]
    (is (= (count s) 2))
    (is (or (and (= (first s) {:s :oak   :x 0 :y 0 :z 1})
                 (= (fnext s) {:s :maple :x 0 :y 2 :z 2}))
            (and (= (fnext s) {:s :oak   :x 0 :y 0 :z 1})
                 (= (first s) {:s :maple :x 0 :y 2 :z 2}))))))
(deftest test-view
  (doseq [db [map-database par-database] :let [v (view db :insts [1 3])]]
    (is (= (freeze v) #{[13 88] [87 55] [74 31] [54 5] [9 12]}))))
(deftest test-edit
  (is (= (edit [:a :b :c] 3 :d) [:a :b :c :d]))
  (is (= (edit [:a :b :c] [1 2] [:x :y]) [:a :x :y]))
  (let [d {:a [0 1] :b [2 3] :c [4 5]}]
    (is (= (edit d all 0 {:a 10 :b 11 :c 12})
           {:a [10 1] :b [11 3] :c [12 5]}))
    (is (= (edit d :d [9 9]) (assoc d :d [9 9])))
    (is (= (edit d all 1 del) {:a [0] :b [2] :c [4]}))))
(deftest test-edit-view
  (let [v (view par-database :insts [1 3])
        v0 (freeze v)
        _  (is (= v0 #{[13 88] [87 55] [74 31] [54 5] [9 12]}))
        v1 (freeze (dosync (edit v 1 0)))
        _  (is (= v1 #{[13 0] [87 0] [74 0] [54 0] [9 0]}))
        v2 (part par-database :insts [1 3])]
    (is (= v2 #{[13 0] [87 0] [74 0] [54 0] [9 0]}))))
    
  



