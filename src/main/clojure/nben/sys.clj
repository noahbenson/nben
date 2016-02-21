;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys.clj, part of nben, a mathematics library for the JVM.
;; This namespace imports a large set of useful functions from various libraries and source files,
;; all of which have to do with system interaction.
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

(ns nben.sys
  (:use nben.util)
  (:refer-clojure :exclude [second parents extend name minus import])

  (:require [clj-time.core :as clj-time-core])
  (:require [clj-time.coerce :as clj-time-coerce])
  (:require [clj-time.local :as clj-time-local])
  (:require [clj-time.periodic :as clj-time-periodic])
  (:require [clj-time.predicates :as clj-time-predicates])

  (:require [org.clojars.smee.binary.core :as smee])

  (:require [me.raynes.fs :as fs])
  (:require [clojure.java.io :as io]))

;; --> time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-all-vars clj-time.core :exclude [second extend minus plus])
;; we rename second to seconds, extend to lengthen, and minus to earlier
(import-fn clj-time-core/second seconds)
(import-fn clj-time-core/minus earlier)
(import-fn clj-time-core/plus later)
(import-fn clj-time-core/extend lengthen)
;; local, periodic, and predicates don't seem to cause problems
(import-all-vars clj-time.local)
(import-all-vars clj-time.periodic)
(import-all-vars clj-time.predicates)
;; the coerce symbols we rewrite
(defn seconds->date
  "(seconds->date t) returns a DateTime instance in the UTC time zone corresponding to the 
     given number of seconds after the Unix epoch."
  [t]
  (clj-time-coerce/from-long (long (* t 1000))))
(defn date->seconds
  "(date->seconds d) converts d to the number of seconds since the Unix epoch."
  [d]
  (/ (clj-time-coerce/to-long d) 1000))
(defn milliseconds->date
  "(milliseconds->date t) returns a DateTime instance in the UTC time zone corresponding to the 
     given number of milliseconds after the Unix epoch."
  [t]
  (clj-time-coerce/from-long (long t)))
(defn date->milliseconds
  "(date->milliseconds d) converts d to the number of milliseconds since the Unix epoch."
  [d]
  (clj-time-coerce/to-long d))

;; --> file system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-all-vars me.raynes.fs :exclude [name parents walk])
(import-fn fs/name path-name)
(import-fn fs/parents path-parents)
(import-fn fs/walk path-walk)
(import-all-vars clojure.java.io :exclude [copy file])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here, we specify the system for file i/o

;; #file-format-of ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti file-format-of
  "(file-format-of file) yields a file format type, as recognized by import and export, or nil if
     no format can be determined. This function is a multimethod that switches on the file extension
     string of the file-name, so additional methods can be added. The returned value should be
     registered with the import and export multi-methods: import-stream and export-stream."
  (fn [flname] (fs/extension flname)))
(defmethod file-format-of :default [_] nil)

;; #auto-file-format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti auto-file-format
  "(auto-file-format object) yields an import/export format type for the given object or nil if it
     is not recognized. This is a multi-method that switches on the class of the object."
  (fn [x] (class x)))
(defmethod auto-file-format :default [_] nil)

;; #import-stream ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti import-stream
  "(import-stream stream fmt opts) is a multi-method that switches on fmt, which is the
     import/export format identifier, and imports the appropriate object from the given stream 
     with the consideration of the given options map (which is extracted from the format specifier
     when passed to import). Any method that overloads this should consume as much of the stream as
     is required to import the given type, or throws an exception if the type is not found."
  (fn [stream fmt opts]
    (let [fmt (if (or (vector? fmt) (seq? fmt)) (first fmt) fmt)]
      (upper-case (if (keyword? fmt) (clojure.core/name fmt) (str fmt))))))
(defmethod import-stream :default [s f opts] (arg-err "Unimplemented format: " f))

;; #export-stream ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti export-stream
  "(export-stream stream obj fmt opts) is a multi-method that switches on fmt, which is the
     import/export format identifier, and exports the given object to the given stream with the
     consideration of the given options map (which is extracted from the format specifier when
     passed to import). Any method that overloads this should write as much data into the stream as
     is required to export the given type, or throw an exception if the type is not found."
  (fn [stream obj fmt opts]
    (let [fmt (if (or (vector? fmt) (seq? fmt)) (first fmt) fmt)]
      (upper-case (if (keyword? fmt) (clojure.core/name fmt) (str fmt))))))
(defmethod export-stream :default [s o f opts] (arg-err "Unimplemented format: " f))

;; #import ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn import-file
  "(import-file file) imports the given file by auto-detecting its file type according to extension
     (see also: file-format-of).
   (import-file file type) imports the given file according to the given type, which may be a vector
     beginning with a type and followed by arguments to the type.
     Options may be passed after type; these are bundled and given to the importer.

   In order to register import and export types, see the import-stream and export-stream functions."
  ([fl] (import-file fl nil))
  ([fl fmt & {:as opts}]
     (let [fmt (or fmt (file-format-of fl) (arg-err "cannot determine format of file " fl))]
       (with-open [s (io/input-stream fl)]
         (import-stream s fmt opts)))))

;; #export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn export-file
  "(export-file file obj) exports the given file by auto-detecting its file type according to
     extension and the contents of obj (see also: file-format-of). The object type is only used when
      the file extension does not yield an obvious match (see auto-file-format).
   (export-file file obj type) exports the given object to the given file according to the given
     format type, which may be a vector beginning with a type and followed by arguments to the type.
     Options may be passed after type; these are bundled and given to the exporter.

   In order to register export and export types, see the import-stream and export-stream functions."
  ([fl obj] (export-file fl obj nil))
  ([fl obj fmt & {:as opts}]
     (let [fmt (or fmt (file-format-of fl) (auto-file-format obj)
                   (arg-err "cannot determine format of file " fl))]
       (with-open [s (io/output-stream fl)]
         (export-stream s obj fmt opts)))))


;; #clock ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clock
  "(clock) yields the system clock time in seconds as either an integer or ratio that is precise to
     the millisecond."
  []
  (/ (System/currentTimeMillis) 1000))

