;; Copyright (C) 2013, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns wagjo.data.object
  "Object manipulation."
  (:refer-clojure :exclude [empty empty? count get get-in assoc
                            dissoc update-in assoc! dissoc!
                            contains? merge assoc-in pr-str])
  (:require [goog.object :as go]
            [wagjo.data.array :as ua]
            [wagjo.data.string :as us]))

;;;; Implementation details

(def ^:private nf-sentinel (js-obj))

;;;; Public API

(defn pr-str
  "Prints object to string."
  [o]
  (let [key-arr (go/getKeys o)
        map-fn #(us/cat3 (clojure.core/pr-str %)
                         " "
                         (clojure.core/pr-str (aget o %)))
        entry-arr (ua/mape map-fn key-arr)]
    (if (ua/empty? entry-arr)
      "#<Object{}>"
      (us/cat3 "#<Object{"
               (ua/reduce2 #(us/cat3 %1 ", " %2) entry-arr)
               "}>"))))

;;; Creation

(defn empty
  "Returns an empty object."
  []
  (js* "{}"))

;;; Access

(defn ^boolean object?
  "Returns true if o is an object."
  [o]
  (when-not (nil? o)
    (identical? (type o) js/Object)))

(defn ^boolean empty?
  "Returns true if o is empty object or nil.
  Faster variant of clojure.core/empty?."
  [o]
  (or (nil? o)
      (go/isEmpty o)))

(defn count
  "Returns the number of items in the object.
  (count nil) returns 0. Faster variant of clojure.core/count."
  [o]
  (if (nil? o)
    0
    (go/getCount o)))

(defn get
  "Returns value for a given key. Returns nil if not found.
  Faster variant of clojure.core/get."
  [o key]
  (when-not (nil? o)
    (aget o key)))

(defn get*
  "Returns the value for a given key or not-found, if key not present.
  Faster variant of clojure.core/get."
  [o key not-found]
  (if (nil? o)
    not-found
    (go/get o key not-found)))

(defn get-checked
  "Returns the value at the index.
  Throws exception if key not present.
  Faster variant of clojure.core/get."
  [o key]
  (when-not (nil? o)
    (let [x (go/get o key nf-sentinel)]
      (if (identical? x nf-sentinel)
        (throw "get: key not present!")
        x))))

(defn get-in
  "Returns the value for given keys in nested objects.
  Returns nil if not found.
  Faster variant of clojure.core/get-in."
  [o keys]
  (when-not (nil? o)
    (let [not-found (reduced false)]
      (reduce #(get* %1 %2 not-found) o keys))))

(defn get-in*
  "Returns the value for given keys in nested objects or
  not-found, if key not present.
  Faster variant of clojure.core/get-in."
  [o keys not-found]
  (if (nil? o)
    not-found
    (let [not-found (reduced not-found)]
      (reduce #(get* %1 %2 not-found) o keys))))

(defn get-in-checked
  "Returns the value for given keys in nested objects.
  Throws if not found.
  Faster variant of clojure.core/get-in."
  [o keys]
  (when-not (nil? o)
    (reduce #(if (nil? %1)
               (throw "get-in: key not present")
               (get-checked %1 %2)) o keys)))

(defn ^boolean contains?
  "Returns true if key is present in o, false otherwise.
  Faster variant of clojure.core/contains?."
  [o key]
  (when-not (nil? o)
    (go/containsKey o key)))

(defn ^boolean contains-in?
  "Returns true if keys are present in nested objects,
  false otherwise."
  [o keys]
  (when-not (nil? o)
    (not (identical? (get-in* o keys nf-sentinel) nf-sentinel))))

;;; Mutable Modify

(defn clone
  "Returns cloned copy of object. Returns nil if o is nil."
  [o]
  ;; slow!
  (when-not (nil? o)
    (go/clone o)))

(defn assoc!
  "Sets the val at key. Mutates input object. Returns nil."
  [o key val]
  (aset o key val)
  nil)

(defn dissoc!
  "Removes entry for key. Mutates input array. Returns nil"
  [o key]
  (go/remove o key)
  nil)

(defn update!
  "Sets the val at key. Mutates input array. f is a function that
  will take the old value and return the new value. Returns nil.
  Passes nil in f is entry does not exists"
  [o key f]
  (aset o key (f (aget o key)))
  nil)

;;; Modify

(defn merge
  "Returns new object which is a concatenation of o1 and o2.
  Faster version of clojure.core/merge."
  [o1 o2]
  ;; TODO: how to handle nil?
  (let [o (clone o1)]
    (go/forEach o2 #(aset o %1 %2))
    o))

(defn assoc
  "Assoc[iate]. Returns a new object that contains val for key."
  [o key val]
  (let [newo (or (clone o) (empty))]
    (assoc! newo key val)
    newo))

#(
  (defn assoc-in
    "Assoc[iate]. Returns a new array that contains val at index.
  Note - index must be <= (count array)."
    [a index val]
    ;; handle empty o
    (let [newa (clone a)]
      (assoc! newa index val)
      newa))

  (defn dissoc
    "Assoc[iate]. Returns a new array that contains val at index.
  Note - index must be <= (count array)."
    [a index val]
    (let [newa (clone a)]
      (assoc! newa index val)
      newa))

  (defn dissoc-in
    "Assoc[iate]. Returns a new array that contains val at index.
  Note - index must be <= (count array)."
    [a index val]
    (let [newa (clone a)]
      (assoc! newa index val)
      newa))

  (defn update
    "Returns a new array that contains updated val at index.
  Note - index must be < (count array)."
    [a index f]
    (let [newa (clone a)]
      (update! newa index f)
      newa))

  (defn update-in
    "Returns a new array that contains updated val at index.
  Note - index must be < (count array)."
    [a index f]
    (let [newa (clone a)]
      (update! newa index f)
      newa)))
