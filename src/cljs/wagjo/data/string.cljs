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

(ns wagjo.data.string
  "String manipulation."
  (:refer-clojure :exclude [empty empty? count nth peek pop assoc
                            split-at conj reduce reduce-kv])
  (:require [goog.string :as gs]))

;;;; Public API

;;; Creation

(def ^:const empty-string "")

(defn empty
  "Returns an empty string."
  []
  empty-string)

;;; Access

(defn ^boolean string?
  "Returns true if s is a string."
  [s]
  (clojure.core/string? s))

(defn ^boolean empty?
  "Returns true if s is an empty string or nil.
  Faster variant of clojure.core/empty?"
  [s]
  (or (nil? s)
      (zero? (.-length s))))

(defn count
  "Returns the number of characters in text.
  (count nil) returns 0. Faster variant of clojure.core/count."
  [s]
  (if (nil? s)
    0
    (.-length s)))

(defn nth
  "Returns the character at the index. nth throws an exception
  if index out of bounds. Faster variant of clojure.core/nth"
  [s index]
  (when (not (nil? s))
    (if (or (neg? index) (<= (.-length s) index))
      (throw "nth: Index out of bounds!")
      (.charAt s index))))

(defn nth*
  "Returns the character at the index or not-found, if index
  out of bounds. Faster variant of clojure.core/nth"
  [s index not-found]
  (if (or (nil? s) (neg? index) (<= (.-length s) index))
    not-found
    (.charAt s index)))

(defn nth-unchecked
  "Returns the character at the index. Does not check for boundaries.
  Faster variant of clojure.core/nth"
  [s index]
  (.charAt s index))

(defn peekl
  "Same as, but much more efficient than, first.
  If the string is empty, returns nil."
  [s]
  (when-not (empty? s)
    (.charAt s 0)))

(defn peekr
  "Same as, but much more efficient than, last.
  If the string is empty, returns nil."
  [s]
  (when-not (empty? s)
    (.charAt s (dec (.-length s)))))

(def peek peekr)

(defn peekl-unchecked
  "Same as, but much more efficient than, first.
  Undefined behavior if the string is empty."
  [s]
  (.charAt s 0))

(defn peekr-unchecked
  "Same as, but much more efficient than, last.
  Undefined behavior if the string is empty."
  [s]
  (.charAt s (dec (.-length s))))

(def peek-unchecked peekr-unchecked)

;;; String concatenation

;; NOTE: For string concatenation, please use cat macro.

(defn cat2
  "Returns concatenation of two strings. Is much faster than str."
  [x1 x2]
  (js* "(~{x1} + ~{x2})"))

(defn cat3
  "Returns concatenation of three strings. Is much faster than str."
  [x1 x2 x3]
  (js* "(~{x1} + ~{x2} + ~{x3})"))

(defn cat4
  "Returns concatenation of four strings. Is much faster than str."
  [x1 x2 x3 x4]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4})"))

(defn cat5
  "Returns concatenation of five strings. Is much faster than str."
  [x1 x2 x3 x4 x5]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5})"))

(defn cat6
  "Returns concatenation of six strings. Is much faster than str."
  [x1 x2 x3 x4 x5 x6]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5} + ~{x6})"))

(defn cat7
  "Returns concatenation of seven strings. Is much faster than str."
  [x1 x2 x3 x4 x5 x6 x7]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5} + ~{x6} + ~{x7})"))

(defn cat8
  "Returns concatenation of eight strings. Is much faster than str."
  [x1 x2 x3 x4 x5 x6 x7 x8]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5} + ~{x6} + ~{x7} + ~{x8})"))

(defn cat9
  "Returns concatenation of nine strings. Is much faster than str."
  [x1 x2 x3 x4 x5 x6 x7 x8 x9]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5} + ~{x6} + ~{x7} + ~{x8} + ~{x9})"))

(defn cat10
  "Returns concatenation of ten strings. Is much faster than str."
  [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]
  (js* "(~{x1} + ~{x2} + ~{x3} + ~{x4} + ~{x5} + ~{x6} + ~{x7} + ~{x8} + ~{x9} + ~{x10})"))

(defn cat-seq
  "Returns concatenation of seq of string.
  Is faster than clojure.core/str."
  [s-seq]
  (clojure.core/reduce cat2 "" s-seq))

;;; Modify

(defn slice
  "Returns the substring of a beginning at start inclusive, and ending
  at end, exclusive."
  [s start end]
  (.substring s start end))

(defn slice-from
  "Returns the substring of a beginning at start inclusive, until the
  end of string."
  [s start]
  (.substring s start))

(defn slice-to
  "Returns the substring of a from its start, and ending
  at end, exclusive."
  [s end]
  (.substring s 0 end))

(defn split-at
  "Returns vector of strings produced by splitting s at index pos."
  [s index]
  [(slice-to s index) (slice-from s index)])

;; cat is a macro, defined in a separate file

(defn popl
  "Returns new string without first character. If the string is empty,
  throws an exception."
  [s]
  (if (empty? s)
    (throw "Cannot pop empty or nil string.")
    (.substring s 1)))

(defn popr
  "Returns new string without last character. If the string is empty,
  throws an exception."
  [s]
  (if (empty? s)
    (throw "Cannot pop empty or nil string.")
    (.slice s 0 -1)))

(def pop popr)

(defn popl-unchecked
  "Returns new string without first character.
  Undefined behavior if the string is empty."
  [s]
  (.substring s 1))

(defn popr-unchecked
  "Returns new string without last item.
  Undefined behavior if the string is empty."
  [s]
  (.slice s 0 -1))

(def pop-unchecked popr-unchecked)

(defn assoc
  "Assoc[iate]. Returns a new string that contains c at index.
  Note - index must be <= (count string)."
  [s index c]
  (cat3 (.substring s 0 index) c (.substring s (inc index))))

(defn update
  "Returns a new string that contains updated character at index.
  Note - index must be < (count string)."
  [s index f]
  (cat3 (.substring s 0 index)
        (f (.charAt s index))
        (.substring s (inc index))))

(defn conjl
  "conj[oin]. Returns a new string with c added to the left.
  c may be a character or a string."
  [s c]
  (cat2 c s))

(defn conjr
  "conj[oin]. Returns a new string with var added to the right.
  c may be a character or a string."
  [s c]
  (cat2 s c))

(def conj conjr)

(defn conjl-arr
  "conj[oin]. Returns a new string with c-arr added to the left.
  Order of elements is retained.
  Items in c-arr may be chars or strings."
  [s c-arr]
  (cat2 (cat-seq (seq c-arr)) s))

(defn conjr-arr
  "conj[oin]. Returns a new string with c-arr added to the right.
  Items in c-arr may be chars or strings."
  [s c-arr]
  (cat2 s (cat-seq (seq c-arr))))

(def conj-arr conjr-arr)

(defn splice
  "Returns new string with n chars starting at index pos
  replaced with c."
  [s index n c]
  (cat3 (.substring s 0 index) c (.substring s (+ index n))))

(defn splice-arr
  "Returns new string with n chars starting at index pos
  replaced with chars or strings in c-arr."
  [s index n c-arr]
  (cat3 (.substring s 0 index)
        (cat-seq (seq c-arr))
        (.substring s (+ index n))))

(defn insert-before
  "Returns a new string with c inserted before index.
  c may be a character or a string."
  [s index c]
  (cat3 (.substring s 0 index) c (.substring s index)))

(defn insert-before-arr
  "Returns a new string with s-arr inserted before index.
  Items in c-arr may be chars or strings."
  [s index c-arr]
  (cat3 (.substring s 0 index)
        (cat-seq (seq c-arr))
        (.substring s index)))

(defn remove-at
  "Returns new string with item at index position removed."
  [s index]
  (cat2 (.substring s 0 index) (.substring s (inc index))))

(defn remove-n
  "Returns new string with n items starting at index position
  (index position included) removed."
  [s index n]
  (cat2 (.substring s 0 index) (.substring s (+ index n))))

(defn rip
  "Returns result of ripping string at index pos.
  Returns [pre-s char suf-s]. Throws exception for empty string."
  [s index]
  (cond (empty? s) (throw "Cannot rip empty or nil string!")
        (== (.-length s) 1) [(empty) s (empty)]
        :else  [(.substring s 0 index)
                (.charAt s index)
                (.substrting s (inc index))]))

(defn sew
  "Returns sewed string. Kinda opposite of rip."
  [pre-s c-arr suf-s]
  (cat3 pre-s (cat-seq (seq c-arr)) suf-s))

(defn triml
  "Returns new string with n chars trimmed from left."
  [s n]
  (.substring s n))

(defn trimr
  "Returns new string with n chars trimmed from right."
  [s n]
  (.slice s 0 (- n)))

(defn trim
  "Returns new string with nl chars trimmed from left and nr chars
  trimmed from right."
  [s nl nr]
  (.slice s nl (- nr)))

;;; Use

(defn mape
  "Eager string map. Returns new string.
  f takes element as an argument.
  Eager version of clojure.core/map."
  [f s]
  (when-not (nil? s)
    (loop [i 0
           r ""]
      (if (< i (.-length s))
        (recur (inc i) (cat2 r (f (.charAt s i))))
        r))))

(defn mape-indexed
  "Eager string map-indexed. Returns new string.
  f takes two arguments, index and char.
  Eager version of clojure.core/map-indexed."
  [f s]
  (when-not (nil? s)
    (loop [i 0
           r ""]
      (if (< i (.-length s))
        (recur (inc i) (cat2 r (f i (.charAt s i))))
        r))))

(defn reduce
  "String reduce. Faster variant of clojure.core/reduce."
  [f init s]
  (if (nil? s)
    init
    (loop [val init
           i 0]
      (if (< i (.-length s))
        (let [nval (f val (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce2
  "String reduce without starting value.
  Faster variant of clojure.core/reduce."
  [f s]
  (if (empty? s)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (.charAt s 0)
           i 1]
      (if (< i (.-length s))
        (let [nval (f val (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce-reverse
  "Reverse string reduce. Very fast."
  [f init s]
  (loop [val init
         i (if (nil? s) 0 (.-length s))]
    (if (pos? i)
      (let [i (dec i)
            nval (f val (.charAt s i))]
        (if (reduced? nval)
          @nval
          (recur nval i)))
      val)))

(defn reduce2-reverse
  "Reverse string reduce, without starting value. Very fast."
  [f s]
  (if (empty? s)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (.charAt s (dec (.-length s)))
           i (dec (.-length s))]
      (if (pos? i)
        (let [i (dec i)
              nval (f val (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval i)))
        val))))

(defn reduce-kv
  "String reduce-kv. Faster variant of clojure.core/reduce-kv."
  [f init s]
  (if (nil? s)
    init
    (loop [val init
           i 0]
      (if (< i (.-length s))
        (let [nval (f val i (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce2-kv
  "String reduce-kv without starting value.
  Faster variant of clojure.core/reduce-kv."
  [f s]
  (if (empty? s)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (.charAt s 0)
           i 1]
      (if (< i (.-length s))
        (let [nval (f val i (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval (inc i))))
        val))))

(defn reduce-kv-reverse
  "Reverse string reduce. Very fast."
  [f init s]
  (loop [val init
         i (if (nil? s) 0 (.-length s))]
    (if (pos? i)
      (let [i (dec i)
            nval (f val i (.charAt s i))]
        (if (reduced? nval)
          @nval
          (recur nval i)))
      val)))

(defn reduce2-kv-reverse
  "Reverse string reduce, without starting value. Very fast."
  [f s]
  (if (empty? s)
    (let [r (f)] (if (reduced? r) @r r))
    (loop [val (.charAt s (dec (.-length s)))
           i (dec (.-length s))]
      (if (pos? i)
        (let [i (dec i)
              nval (f val i (.charAt s i))]
          (if (reduced? nval)
            @nval
            (recur nval i)))
        val))))

(defn index-of
  "Returns index of search-string inside string.
  Returns -1 if search-string was not found."
  [string search-string]
  (.indexOf string search-string))

(defn index-of-from
  "Returns index of search-string inside string, starting at
  index-from. Returns -1 if search-string was not found."
  [string search-string index-from]
  (.indexOf string search-string index-from))

;;; Misc

(defn html-escape
  "Returns new string with <, \" and & escaped,
  so it can be used inside HTML."
  [s]
  (gs/htmlEscape s))

(defn whitespace-escape
  "Returns new string with whitespaces escaped,
  so they are preserved inside HTML."
  [s]
  (gs/whitespaceEscape s))

(defn camel-case
  "Returns new camel cased string of a given hyphen cased one."
  [s]
  (gs/toCamelCase s))

(defn hyphen-case
  "Returns new hyphen cased string of a given camel cased one."
  [s]
  (gs/toSelectorCase s))
