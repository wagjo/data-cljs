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

(ns wagjo.data.array
  "Array manipulation."
  (:refer-clojure :exclude [empty empty? count nth peek pop assoc
                            split-at conj reduce reduce-kv assoc!
                            conj! pop! array?]))

;;;; Public API

;;; Creation

(defn empty
  "Returns an empty array."
  []
  (array))

;;; Access

(defn ^boolean array?
  "Returns true if a is an array."
  [a]
  (instance? js/Array a))

(defn ^boolean empty?
  "Returns true if a is empty array or nil.
  Faster variant of clojure.core/empty?."
  [a]
  (or (nil? a)
      (zero? (.-length a))))

(defn count
  "Returns the number of items in the array.
  (count nil) returns 0. Faster variant of clojure.core/count."
  [a]
  (if (nil? a)
    0
    (.-length a)))

(defn nth
  "Returns the value at the index. nth throws an exception if index
  out of bounds. Faster variant of clojure.core/nth"
  [a index]
  (when-not (nil? a)
    (if (or (neg? index) (<= (.-length a) index))
      (throw "nth: Index out of bounds!")
      (aget a index))))

(defn nth*
  "Returns the value at the index or not-found, if index
  out of bounds. Faster variant of clojure.core/nth"
  [a index not-found]
  (if (or (nil? a) (neg? index) (<= (.-length a) index))
    not-found
    (aget a index)))

(defn nth-unchecked
  "Returns the value at the index. Does not check for boundaries.
  Faster variant of clojure.core/nth."
  [a index]
  (aget a index))

(defn peekl
  "Same as, but much more efficient than, first.
  If the array is empty, returns nil."
  [a]
  (when-not (empty? a)
    (aget a 0)))

(defn peekr
  "Same as, but much more efficient than, last.
  If the array is empty, returns nil."
  [a]
  (when-not (empty? a)
    (aget a (dec (.-length a)))))

(def peek peekr)

(defn peekl-unchecked
  "Same as, but much more efficient than, first.
  Undefined behavior if the array is empty."
  [a]
  (aget a 0))

(defn peekr-unchecked
  "Same as, but much more efficient than, last.
  Undefined behavior if the array is empty."
  [a]
  (aget a (dec (.-length a))))

(def peek-unchecked peekr-unchecked)

;;; Mutable Modify

(defn clone
  "Returns cloned copy of array."
  [a]
  (.slice a))

(defn pop!
  "Removes the last item from a. If the array is empty,
  throws an exception. Mutates input array. Returns nil."
  [a]
  (if (empty? a)
    (throw "Cannot pop empty array or nil!")
    (do (.pop a)
        nil)))

(defn assoc!
  "Sets the val at index. Mutates input array.
  Note - index must be <= (count a). Returns nil."
  [a index val]
  (aset a index val)
  nil)

(defn update!
  "Sets the val at index. Mutates input array. f is a function that
  will take the old value and return the new value.
  Note - index must be < (count a). Returns nil."
  [a index f]
  (aset a index (f (aget a index)))
  nil)

(defn conj!
  "Appends x at array a. Mutates input array. Returns nil."
  [a x]
  (.push a x)
  nil)

(defn splice!
  "Less clever array splice. Removes n elements after index
  (including index) before inserting. Inserts just one element
  x at index. Mutates input array. Returns array of removed items."
  [a index n x]
  (.splice a index n x))

(defn splice-seq!
  "Clever array splice. Works just like Array.splice()
  (Changes the content of an array, adding new elements while removing
  old elements). Inserts elements from xs into a at index
  after removing n elements from that position.
  Mutates input array. Returns array of removed items."
  [a index n xs]
  (let [sfn (.-splice a)
        ar (array index n)]
    (.apply sfn a (.concat ar (to-array xs)))))

(defn splice-arr!
  "Clever array splice. Works just like Array.splice()
  (Changes the content of an array, adding new elements while removing
  old elements). Inserts elements from axs into a at index
  after removing n elements from that position.
  Mutates input array. Returns array of removed items."
  [a index n axs]
  (let [sfn (.-splice a)
        ar (array index n)]
    (.apply sfn a (.concat ar (or axs (array))))))

(defn insert-before!
  "Inserts x into array a at index.
  Mutates input array. Returns nil."
  [a index x]
  (.splice a index 0 x)
  nil)

(defn insert-before-seq!
  "Inserts xs into array a at index.
  Mutates input array. Returns nil."
  [a index xs]
  (splice-seq! a index 0 xs)
  nil)

(defn insert-before-arr!
  "Inserts axs represented as array into array a at index.
  Mutates input array. Returns nil."
  [a index axs]
  (splice-arr! a index 0 axs)
  nil)

(defn remove-at!
  "Removes one item from array a at index.
  Mutates input array. Returns the array of removed items."
  [a index]
  (.splice a index 1))

(defn remove-n!
  "Removes n items from array a starting at index.
  Mutates input array. Returns the array of removed items."
  [a index n]
  (.splice a index n))

(defn remove-after!
  "Removes all items from array a after index (including index).
  Mutates input array. Returns the array of removed items."
  [a index]
  (.splice a index))

(defn remove-before!
  "Removes all items from array a after index (including index).
  Mutates input array. Returns the array of removed items."
  [a index]
  (.splice a 0 index))

;;; Modify

(defn slice
  "Returns the subarray of a beginning at start inclusive, and ending
  at end, exclusive."
  [a start end]
  (.slice a start end))

(defn slice-from
  "Returns the subarray of a beginning at start inclusive, until the
  end of array."
  [a start]
  (.slice a start))

(defn slice-to
  "Returns the subarray of a from its start, and ending
  at end, exclusive."
  [a end]
  (.slice a 0 end))

(defn split-at
  "Returns vector of arrays produced by splitting a at index pos."
  [a index]
  [(slice-to a index) (slice-from a index)])

(defn cat
  "Returns new array which is a concatenation of a1 and a2.
  Eager version of clojure.core/concat."
  [a1 a2]
  (.concat a1 (or a2 (array))))

(defn popl
  "Returns new array without first item. If the array is empty,
  throws an exception."
  [a]
  (if (empty? a)
    (throw "Cannot pop empty or nil array.")
    (.slice a 1)))

(defn popr
  "Returns new array without last item. If the array is empty,
  throws an exception."
  [a]
  (if (empty? a)
    (throw "Cannot pop empty or nil array.")
    (.slice a 0 -1)))

(def pop popr)

(defn popl-unchecked
  "Returns new array without first item.
  Undefined behavior if the array is empty."
  [a]
  (.slice a 1))

(defn popr-unchecked
  "Returns new array without last item.
  Undefined behavior if the array is empty."
  [a]
  (.slice a 0 -1))

(def pop-unchecked popr-unchecked)

(defn assoc
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

(defn conjl
  "conj[oin]. Returns a new array with var added to the left."
  [a val]
  (let [newa (clone a)]
    (insert-before! newa 0 val)
    newa))

(defn conjr
  "conj[oin]. Returns a new array with var added to the right."
  [a val]
  (let [newa (clone a)]
    (conj! newa val)
    newa))

(def conj conjr)

(defn conjl-arr
  "conj[oin]. Returns a new array with var-arr added to the left.
  Order of elements is retained."
  [a val-arr]
  (cat val-arr a))

(defn conjr-arr
  "conj[oin]. Returns a new array with var-arr added to the right."
  [a val-arr]
  (cat a val-arr))

(def conj-arr conjr-arr)

(defn splice
  "Returns new array with n items starting at index pos
  replaced with val."
  [a index n val]
  (let [newa (clone a)]
    (splice! newa index n val)
    newa))

(defn splice-arr
  "Returns new array with n items starting at index pos
  replaced with items in val-arr."
  [a index n val-arr]
  (let [newa (clone a)]
    (splice-arr! newa index n val-arr)
    newa))

(defn insert-before
  "Returns a new array with val inserted before index."
  [a index val]
  (let [newa (clone a)]
    (insert-before! newa index val)
    newa))

(defn insert-before-arr
  "Returns a new array with val-arr inserted before index."
  [a index val-arr]
  (let [newa (clone a)]
    (insert-before-arr! newa index val-arr)
    newa))

(defn remove-at
  "Returns new array with item at index position removed."
  [a index]
  (let [newa (clone a)]
    (remove-at! newa index)
    newa))

(defn remove-n
  "Returns new array with n items starting at index position
  (index position included) removed."
  [a index n]
  (let [newa (clone a)]
    (remove-n! newa index n)
    newa))

(defn rip
  "Returns result of ripping array at index pos.
  Returns [pre-arr item suf-arr]. Throws exception for empty array."
  [a index]
  (cond (empty? a) (throw "Cannot rip empty or nil array!")
        (== (.-length a) 1) [(empty) (aget a 0) (empty)]
        :else  [(slice-to a index)
                (aget a index)
                (slice-from a (inc index))]))

(defn sew
  "Returns sewed array. Kinda opposite of rip."
  [pre-arr item-arr suf-arr]
  (.concat pre-arr (or item-arr (array)) (or suf-arr (array))))

(defn triml
  "Returns new array with n items trimmed from left."
  [a n]
  (.slice a n))

(defn trimr
  "Returns new array with n items trimmed from right."
  [a n]
  (.slice a 0 (- n)))

(defn trim
  "Returns new array with nl items trimmed from left and nr items
  trimmed from right."
  [a nl nr]
  (.slice a nl (- nr)))

;;; Use

(defn mape
  "Eager array map. Returns new array.
  f takes element as an argument.
  Eager version of clojure.core/map."
  [f a]
  (when-not (nil? a)
    (let [a (aclone a)]
      (loop [i 0]
        (if (< i (.-length a))
          (let [nval (f (aget a i))]
            (aset a i nval)
            (recur (inc i)))
          a)))))

(defn mape-indexed
  "Eager array map-indexed. Returns new array.
  f takes two arguments, index and element.
  Eager version of clojure.core/map-indexed."
  [f a]
  (when-not (nil? a)
    (let [a (aclone a)]
      (loop [i 0]
        (if (< i (.-length a))
          (let [nval (f i (aget a i))]
            (aset a i nval)
            (recur (inc i)))
          a)))))

(defn reduce
  "Array reduce. Faster variant of clojure.core/reduce."
  [f init a]
  (if (nil? a)
    init
    (loop [val init
           i 0]
      (if (< i (.-length a))
        (let [nval (f val (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce2
  "Array reduce without starting value.
  Faster variant of clojure.core/reduce."
  [f a]
  (if (empty? a)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (aget a 0)
           i 1]
      (if (< i (.-length a))
        (let [nval (f val (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce-reverse
  "Reverse array reduce. Very fast."
  [f init a]
  (loop [val init
         i (if (nil? a) 0 (.-length a))]
    (if (pos? i)
      (let [i (dec i)
            nval (f val (aget a i))]
        (if (reduced? nval)
          @nval
          (recur nval i)))
      val)))

(defn reduce2-reverse
  "Reverse array reduce without starting value.
  Very fast."
  [f a]
  (if (empty? a)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (aget a (dec (.-length a)))
           i (dec (.-length a))]
      (if (pos? i)
        (let [i (dec i)
              nval (f val (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval i)))
        val))))

(defn reduce-kv
  "Array reduce-kv. Faster variant of clojure.core/reduce-kv."
  [f init a]
  (if (nil? a)
    init
    (loop [val init
           i 0]
      (if (< i (.-length a))
        (let [nval (f val i (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce2-kv
  "Array reduce-kv, without starting value.
  Faster variant of clojure.core/reduce-kv."
  [f a]
  (if (empty? a)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (aget a 0)
           i 1]
      (if (< i (.-length a))
        (let [nval (f val i (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce-kv-reverse
  "Reverse array reduce. Very fast."
  [f init a]
  (loop [val init
         i (if (nil? a) 0 (.-length a))]
    (if (pos? i)
      (let [i (dec i)
            nval (f val i (aget a i))]
        (if (reduced? nval)
          @nval
          (recur nval i)))
      val)))

(defn reduce2-kv-reverse
  "Reverse array reduce. Very fast."
  [f a]
  (if (empty? a)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (aget a (dec (.-length a)))
           i (dec (.-length a))]
      (if (pos? i)
        (let [i (dec i)
              nval (f val i (aget a i))]
          (if (reduced? nval)
            @nval
            (recur nval i)))
        val))))

(defn index-of
  "Returns index of val inside a.
  Returns -1 if val was not found."
  [a val]
  (.indexOf a val))

(defn index-of-from
  "Returns index of val inside a, starting at
  index-from. Returns -1 if val was not found."
  [a val index-from]
  (.indexOf a val index-from))
