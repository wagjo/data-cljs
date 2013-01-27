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

(ns wagjo.data.vector
  "Vector manipulation."
  (:refer-clojure :exclude [empty empty? count nth peek pop assoc
                            split-at conj reduce reduce-kv]))

;;;; Implementation details

(def ^:const empty-vector [])

;;;; Public API

;;; Creation

(defn empty
  "Returns an empty vector."
  []
  empty-vector)

;;; Access

(defn ^boolean empty?
  "Returns true if v is empty vector or nil.
  Faster variant of clojure.core/empty?."
  [v]
  (or (nil? v)
      (not (-seq v))))

(defn count
  "Returns the number of items in the vector.
  (count nil) returns 0. Faster variant of clojure.core/count."
  [v]
  (if (nil? v)
    0
    (-count v)))

(defn nth
  "Returns the value at the index. nth throws an exception if index
  out of bounds. Faster variant of clojure.core/nth"
  [v index]
  (when-not (nil? v)
    (if (or (neg? index) (<= (-count v) index))
      (throw "nth: Index out of bounds!")
      (-nth v index))))

(defn nth*
  "Returns the value at the index or not-found, if index
  out of bounds. Faster variant of clojure.core/nth"
  [v index not-found]
  (if (nil? v)
    not-found
    (-nth v index not-found)))

(defn nth-unchecked
  "Returns the value at the index. Does not check for boundaries.
  Faster variant of clojure.core/nth."
  [v index]
  (-nth v index))

(defn peekl
  "Same as, but much more efficient than, first.
  If the vector is empty, returns nil."
  [v]
  (nth* v 0 nil))

(defn peekr
  "Same as, but much more efficient than, last.
  If the vector is empty, returns nil."
  [v]
  (when-not (nil? v)
    (-peek v)))

(def peek peekr)

(defn peekl-unchecked
  "Same as, but much more efficient than, first.
  Undefined behavior if the vector is empty."
  [v]
  (-nth v 0))

(defn peekr-unchecked
  "Same as, but much more efficient than, last.
  Undefined behavior if the vector is empty."
  [v]
  (-peek v))

(def peek-unchecked peekr-unchecked)

;;; Modify

(defn slice
  "Returns the subvec of v beginning at start inclusive, and ending
  at end, exclusive."
  [v start end]
  (subvec v start end))

(defn slice-from
  "Returns the subvec of v beginning at start inclusive, until the
  end of vector."
  [v start]
  (subvec v start))

(defn slice-to
  "Returns the subvec of v from its start, and ending
  at end, exclusive."
  [v end]
  (subvec v 0 end))

(defn split-at
  "Returns vector of vectors produced by splitting v at index pos."
  [v index]
  [(slice-to v index) (slice-from v index)])

(defn cat
  "Returns new vector which is a concatenation of v1 and v2.
  Eager version of clojure.core/concat.
  Very slow!"
  [v1 v2]
  ;; TODO: use flexvec here
  (into v1 v2))

(defn popl
  "Returns new vector without first item. If the vector is empty,
  throws an exception."
  [v]
  (if (empty? v)
    (throw "Cannot pop empty or nil vector.")
    (subvec v 1)))

(defn popr
  "Returns new vector without last item. If the vector is empty,
  throws an exception."
  [v]
  (if (nil? v)
    (throw "Cannot pop nil.")
    (-pop v)))

(def pop popr)

(defn popl-unchecked
  "Returns new vector without first item.
  Undefined behavior if the vector is empty."
  [v]
  (subvec v 1))

(defn popr-unchecked
  "Returns new vector without last item.
  Undefined behavior if the vector is empty."
  [v]
  (-pop v))

(def pop-unchecked popr-unchecked)

(defn assoc
  "Assoc[iate]. Returns a new vector that contains val at index.
  Note - index must be <= (count vector)."
  [v index val]
  (-assoc v index val))

(defn update
  "Returns a new vector that contains updated val at index.
  Note - index must be < (count vector)."
  [v index f]
  (-assoc v index (f (-nth v index))))

(defn conjl
  "conj[oin]. Returns a new vector with var added to the left.
  Very slow!"
  [v val]
  ;; TODO: use flexvec here
  (into [val] v))

(defn conjr
  "conj[oin]. Returns a new vector with var added to the right."
  [v val]
  (-conj v val))

(def conj conjr)

(defn conjl-arr
  "conj[oin]. Returns a new vector with var-arr added to the left.
  Order of elements is retained.
  Very slow!"
  [v val-arr]
  ;; TODO: use flexvec here
  (into (vec (seq val-arr)) v))

(defn conjr-arr
  "conj[oin]. Returns a new vector with var-arr added to the right."
  [v val-arr]
  (into v (seq val-arr)))

(def conj-arr conjr-arr)

(defn splice
  "Returns new vector with n items starting at index pos
  replaced with val.
  Very slow!"
  [v index n val]
  ;; TODO: use flexvec here
  (let [pre (subvec v 0 index)
        suf (subvec v (+ index n))]
    (into (conj (or pre (empty)) val) suf)))

(defn splice-arr
  "Returns new vector with n items starting at index pos
  replaced with items in val-arr.
  Very slow!"
  [v index n val-arr]
  ;; TODO: use flexvec here
  (let [pre (subvec v 0 index)
        suf (subvec v (+ index n))]
    (into (into (or pre (empty)) (seq val-arr)) suf)))

(defn insert-before
  "Returns a new vector with val inserted before index."
  [v index val]
  (splice v index 0 val))

(defn insert-before-arr
  "Returns a new vector with val-arr inserted before index."
  [v index val-arr]
  (splice-arr v index 0 val-arr))

(defn remove-at
  "Returns new vector with item at index position removed.
  Very slow!"
  [v index]
  ;; TODO: use flexvec here
  (let [pre (subvec v 0 index)
        suf (subvec v (inc index))]
    (into (or pre (empty)) suf)))

(defn remove-n
  "Returns new vector with n items starting at index position
  (index position included) removed.
  Very slow!"
  [v index n]
  ;; TODO: use flexvec here
  (let [pre (subvec v 0 index)
        suf (subvec v (+ index n))]
    (into (or pre (empty)) suf)))

(defn rip
  "Returns result of ripping vector at index pos.
  Returns [pre-vec item suf-vec]. Throws exception for empty vector."
  [v index]
  (cond (empty? v) (throw "Cannot rip empty vector or nil!")
        (== (-count v) 1) [(empty) (-peek v) (empty)]
        :else  [(slice-to v index)
                (nth-unchecked v index)
                (slice-from v (inc index))]))

(defn sew
  "Returns sewed vector. Kinda opposite of rip.
  Very slow!"
  ;; TODO: use flexvec here
  [pre-vec item-arr suf-vec]
  (into (into pre-vec (seq item-arr)) suf-vec))

(defn triml
  "Returns new vector with n items trimmed from left."
  [v n]
  (slice-from v n))

(defn trimr
  "Returns new vector with n items trimmed from right."
  [v n]
  (slice-to v (- (-count v) n)))

(defn trim
  "Returns new vector with nl items trimmed from left and nr items
  trimmed from right."
  [v nl nr]
  (slice v nl (- (-count v) nr)))

;;; Use

(defn mape
  "Eager vector map. Returns new vector.
  f takes element as an argument.
  Eager version of clojure.core/map.
  Pretty slow."
  [f v]
  (vec (map f v)))

(defn mape-indexed
  "Eager vector map-indexed. Returns new vector.
  f takes two arguments, index and element.
  Eager version of clojure.core/map-indexed.
  Pretty slow."
  [f v]
  (vec (map-indexed f v)))

(defn reduce
  "Vector reduce. Faster variant of clojure.core/reduce."
  [f init v]
  (-reduce v f init))

(defn reduce2
  "Vector reduce without starting value.
  Faster variant of clojure.core/reduce."
  [f v]
  (-reduce v f))

(defn reduce-reverse
  "Reverse vector reduce. Very slow."
  [f init v]
  (reduce f init (rseq v)))

(defn reduce2-reverse
  "Reverse vector reduce without starting value. Very slow."
  [f v]
  (reduce f (rseq v)))

(defn reduce-kv
  "Vector reduce-kv. Faster variant of clojure.core/reduce-kv."
  [f init v]
  (-kv-reduce v f init))

(defn reduce2-kv
  "Vector reduce-kv without starting value.
  Faster variant of clojure.core/reduce-kv."
  [f v]
  (if (empty? v)
    (let [r (f)] (if (reduced? r) @r r))
    (-kv-reduce (popl v) f (peekl v))))

(defn reduce-kv-reverse
  "Reverse vector reduce. Very slow."
  [f init v]
  (-kv-reduce (vec (rseq v)) f init))

(defn reduce2-kv-reverse
  "Reverse vector reduce-kv without starting value. Very slow."
  [f v]
  (if (empty? v)
    (let [r (f)] (if (reduced? r) @r r))
    (-kv-reduce (vec (rseq (popr v))) f (peekr v))))

(defn index-of
  "Returns index of val inside v.
  Returns -1 if val was not found."
  [v val]
  (let [f (fn [_ index x] (if (identical? val x) (reduced index) -1))]
    (reduce-kv f -1 v)))

(defn index-of-from
  "Returns index of val inside v, starting at
  index-from. Returns -1 if val was not found."
  [v val index-from]
  (let [t (slice-from v index-from)
        f (fn [_ index x] (if (identical? val x)
                            (reduced (+ index index-from))
                            -1))]
    (reduce-kv f -1 v)))
