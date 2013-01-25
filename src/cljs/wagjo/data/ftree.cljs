;; Copyright (c) Rich Hickey, Chris Houser. All rights reserved.
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

(ns wagjo.data.ftree
  "Finger tree data structure."
  (:require-macros [wagjo.data.ftree :as ftm])
  (:refer-clojure :exclude [empty empty? count nth peek pop assoc
                            conj split-at reduce reduce-kv assoc!
                            conj! pop!])
  (:require [wagjo.data.array :as ua]))

;;;; Implementation details

(declare empty-tree single-tree deep-tree
         digit1 digit2 digit3 digit4 dg-4?
         digit2-l digit2-r digit3-l digit3-r digit4-l digit4-r
         transient-digit transient-empty-tree
         transient-single-tree transient-deep-tree)

;;; Measured

(defprotocol IMeasured
  "Something which can be measured."
  (-measured [t] "Returns measured value of t."))

(extend-type default
  IMeasured
  (-measured [_] 1))

;;; Digits

(deftype Digit1 [a measure-ref]
  Object
  (toString [o]
    (pr-str o))

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<dg1[" (pr-str @measure-ref) "] "
                    (pr-str a) ">")))

  IMeasured
  (-measured [_] @measure-ref)

  Object
  (dg-count [_] @measure-ref)
  (dg-conjl [_ o] (digit2-l o a measure-ref))
  (dg-conjr [_ o] (digit2-r a o measure-ref))
  (dg-peekl [_] a)
  (dg-peekr [_] a)
  (dg-popl [_] nil)
  (dg-popr [_] nil)
  (dg-measured-popl [_] 0)
  (dg-measured-popr [_] 0)
  (dg-split [_ _ _] [nil a nil])
  (dg-nth [_ _ _] [0 a])
  (dg-update [_ index f acc nest]
    (if (zero? nest)
      (digit1 (f a))
      (digit1 (.dg-update a index f acc (dec nest)))))
  (dg-map [_ f nest]
    (if (zero? nest)
      (digit1 (f a))
      (digit1 (.dg-map a f (dec nest)))))
  (dg-map-indexed [_ f index nest]
    (if (zero? nest)
      (digit1 (f index a))
      (digit1 (.dg-map-indexed a f index (dec nest)))))
  (dg-reduce [_ pred-fn start nest]
    (if (zero? nest)
      (pred-fn start a)
      (.dg-reduce a pred-fn start (dec nest))))
  (dg-reduce-reverse [_ pred-fn start nest]
    (if (zero? nest)
      (pred-fn start a)
      (.dg-reduce-reverse a pred-fn start (dec nest))))
  (dg-reduce-kv [_ pred-fn start index nest]
    (if (zero? nest)
      (pred-fn start index a)
      (.dg-reduce-kv a pred-fn start index (dec nest))))
  (dg-reduce-kv-reverse [_ pred-fn start index nest]
    (if (zero? nest)
      (pred-fn start index a)
      (.dg-reduce-kv-reverse a pred-fn start index (dec nest))))
  (dg-transient [_] (transient-digit 1 a nil nil nil))
  (dg-tree [_] (single-tree a))
  (dg-aconcat [_ arr] (ua/conj! arr a) arr))

(deftype Digit2 [a b measure-ref]
  Object
  (toString [o]
    (pr-str o))

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<dg2[" (pr-str @measure-ref) "] "
                    (pr-str a) " " (pr-str b) ">")))

  IMeasured
  (-measured [_] @measure-ref)

  Object
  (dg-count [_] @measure-ref)
  (dg-conjl [_ o] (digit3-l o a b measure-ref))
  (dg-conjr [_ o] (digit3-r a b o measure-ref))
  (dg-peekl [_] a)
  (dg-peekr [_] b)
  (dg-popl [_] (digit1 b))
  (dg-popr [_] (digit1 a))
  (dg-measured-popl [_] (-measured b))
  (dg-measured-popr [_] (-measured a))
  (dg-split [_ pred-fn acc]
    (let [acc (+ acc (-measured a))]
      (if (pred-fn acc)
        [nil a (digit1 b)]
        [(digit1 a) b nil])))
  (dg-nth [_ index acc]
    (let [ma (-measured a)
          acc (+ acc ma)]
      (if (> acc index)
        [0 a]
        [ma b])))
  (dg-update [_ index f old-acc nest]
    (let [ma (-measured a)
          acc (+ old-acc ma)
          fx (fn [ml x] (if (zero? nest)
                          (f x)
                          (.dg-update x index f
                                      (+ old-acc ml) (dec nest))))]
      (if (> acc index)
        (digit2 (fx 0 a) b)
        (digit2 a (fx ma b)))))
  (dg-map [_ f nest]
    (if (zero? nest)
      (digit2 (f a) (f b))
      (digit2 (.dg-map a f (dec nest)) (.dg-map b f (dec nest)))))
  (dg-map-indexed [_ f index nest]
    (if (zero? nest)
      (digit2 (f index a) (f (inc index) b))
      (digit2 (.dg-map-indexed a f index (dec nest))
              (.dg-map-indexed b f
                               (+ index (.dg-count a)) (dec nest)))))
  (dg-reduce [_ pred-fn start nest]
    (if (zero? nest)
      (let [ra (pred-fn start a)]
        (if (reduced? ra)
          ra
          (pred-fn ra b)))
      (let [nest (dec nest)
            ra (.dg-reduce a pred-fn start nest)]
        (if (reduced? ra)
          ra
          (.dg-reduce b pred-fn ra nest)))))
  (dg-reduce-reverse [_ pred-fn start nest]
    (if (zero? nest)
      (let [rb (pred-fn start b)]
        (if (reduced? rb)
          rb
          (pred-fn rb a)))
      (let [nest (dec nest)
            rb (.dg-reduce-reverse b pred-fn start nest)]
        (if (reduced? rb)
          rb
          (.dg-reduce-reverse a pred-fn rb nest)))))
  (dg-reduce-kv [_ pred-fn start index nest]
    (if (zero? nest)
      (let [ra (pred-fn start index a)]
        (if (reduced? ra)
          ra
          (pred-fn ra (inc index) b)))
      (let [nest (dec nest)
            ra (.dg-reduce-kv a pred-fn start index nest)]
        (if (reduced? ra)
          ra
          (.dg-reduce-kv b pred-fn ra
                         (+ index (.dg-count a)) nest)))))
  (dg-reduce-kv-reverse [_ pred-fn start index nest]
    (if (zero? nest)
      (let [rb (pred-fn start index b)]
        (if (reduced? rb)
          rb
          (pred-fn rb (dec index) a)))
      (let [nest (dec nest)
            rb (.dg-reduce-kv-reverse b pred-fn start index nest)]
        (if (reduced? rb)
          rb
          (.dg-reduce-kv-reverse a pred-fn rb
                                 (- index (.dg-count b)) nest)))))
  (dg-transient [_] (transient-digit 2 a b nil nil))
  (dg-tree [_] (deep-tree (digit1 a) empty-tree (digit1 b)))
  (dg-aconcat [_ arr] (ua/conj! arr a) (ua/conj! arr b) arr))

(deftype Digit3 [a b c measure-ref]
  Object
  (toString [o]
    (pr-str o))

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<dg3[" (pr-str @measure-ref) "] "
                    (pr-str a) " " (pr-str b) " " (pr-str c) ">")))

  IMeasured
  (-measured [_] @measure-ref)

  Object
  (dg-count [_] @measure-ref)
  (dg-conjl [_ o] (digit4-l o a b c measure-ref))
  (dg-conjr [_ o] (digit4-r a b c o measure-ref))
  (dg-peekl [_] a)
  (dg-peekr [_] c)
  (dg-popl [_] (digit2 b c))
  (dg-popr [_] (digit2 a b))
  (dg-measured-popl [_] (+ (-measured b) (-measured c)))
  (dg-measured-popr [_] (+ (-measured a) (-measured b)))
  (dg-split [_ pred-fn acc]
    (let [acc (+ acc (-measured a))]
      (if (pred-fn acc)
        [nil a (digit2 b c)]
        (let [acc (+ acc (-measured b))]
          (if (pred-fn acc)
            [(digit1 a) b (digit1 c)]
            [(digit2 a b) c nil])))))
  (dg-nth [_ index acc]
    (let [ma (-measured a)
          acc (+ acc ma)]
      (if (> acc index)
        [0 a]
        (let [mb (-measured b)
              acc (+ acc mb)]
          (if (> acc index)
            [ma b]
            [(+ ma mb) c])))))
  (dg-update [_ index f old-acc nest]
    (let [ma (-measured a)
          acc (+ old-acc ma)
          fx (fn [ml x] (if (zero? nest)
                          (f x)
                          (.dg-update x index f
                                      (+ old-acc ml) (dec nest))))]
      (if (> acc index)
        (digit3 (fx 0 a) b c)
        (let [mb (-measured b)
              acc (+ acc mb)]
          (if (> acc index)
            (digit3 a (fx ma b) c)
            (digit3 a b (fx (+ ma mb) c)))))))
  (dg-map [_ f nest]
    (if (zero? nest)
      (digit3 (f a) (f b) (f c))
      (digit3 (.dg-map a f (dec nest))
              (.dg-map b f (dec nest))
              (.dg-map c f (dec nest)))))
  (dg-map-indexed [_ f index nest]
    (if (zero? nest)
      (digit3 (f index a) (f (inc index) b) (f (+ index 2) c))
      (digit3 (.dg-map-indexed a f index (dec nest))
              (.dg-map-indexed b f (+ index (.dg-count a)) (dec nest))
              (.dg-map-indexed c f (+ index (.dg-count a)
                                      (.dg-count b)) (dec nest)))))
  (dg-reduce [_ pred-fn start nest]
    (if (zero? nest)
      (let [ra (pred-fn start a)]
        (if (reduced? ra)
          ra
          (let [rb (pred-fn ra b)]
            (if (reduced? rb)
              rb
              (pred-fn rb c)))))
      (let [nest (dec nest)
            ra (.dg-reduce a pred-fn start nest)]
        (if (reduced? ra)
          ra
          (let [rb (.dg-reduce b pred-fn ra nest)]
            (if (reduced? rb)
              rb
              (.dg-reduce c pred-fn rb nest)))))))
  (dg-reduce-reverse [_ pred-fn start nest]
    (if (zero? nest)
      (let [rc (pred-fn start c)]
        (if (reduced? rc)
          rc
          (let [rb (pred-fn rc b)]
            (if (reduced? rb)
              rb
              (pred-fn rb a)))))
      (let [nest (dec nest)
            rc (.dg-reduce-reverse c pred-fn start nest)]
        (if (reduced? rc)
          rc
          (let [rb (.dg-reduce-reverse b pred-fn rc nest)]
            (if (reduced? rb)
              rb
              (.dg-reduce-reverse a pred-fn rb nest)))))))
  (dg-reduce-kv [_ pred-fn start index nest]
    (if (zero? nest)
      (let [ra (pred-fn start index a)]
        (if (reduced? ra)
          ra
          (let [rb (pred-fn ra (inc index) b)]
            (if (reduced? rb)
              rb
              (pred-fn rb (+ index 2) c)))))
      (let [nest (dec nest)
            ra (.dg-reduce-kv a pred-fn start index nest)]
        (if (reduced? ra)
          ra
          (let [rb (.dg-reduce-kv b pred-fn ra
                                  (+ index (.dg-count a)) nest)]
            (if (reduced? rb)
              rb
              (.dg-reduce-kv c pred-fn rb
                             (+ index (.dg-count a) (.dg-count b))
                             nest)))))))
  (dg-reduce-kv-reverse [_ pred-fn start index nest]
    (if (zero? nest)
      (let [rc (pred-fn start index c)]
        (if (reduced? rc)
          rc
          (let [rb (pred-fn rc (dec index) b)]
            (if (reduced? rb)
              rb
              (pred-fn rb (- index 2) a)))))
      (let [nest (dec nest)
            rc (.dg-reduce-kv-reverse c pred-fn start index nest)]
        (if (reduced? rc)
          rc
          (let [rb (.dg-reduce-kv-reverse b pred-fn rc
                                          (- index (.dg-count c))
                                          nest)]
            (if (reduced? rb)
              rb
              (.dg-reduce-kv-reverse a pred-fn rb
                                     (- index
                                        (.dg-count c) (.dg-count b))
                                     nest)))))))
  (dg-transient [_] (transient-digit 3 a b c nil))
  (dg-tree [_] (deep-tree (digit1 a) empty-tree (digit2 b c)))
  (dg-aconcat [_ arr]
    (ua/conj! arr a) (ua/conj! arr b) (ua/conj! arr c) arr))

(deftype Digit4 [a b c d measure-ref]
  Object
  (toString [o]
    (pr-str o))

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<dg4[" (pr-str @measure-ref) "] "
                    (pr-str a) " " (pr-str b) " "
                    (pr-str c) " " (pr-str d) ">")))

  IMeasured
  (-measured [_] @measure-ref)

  Object
  (dg-count [_] @measure-ref)
  (dg-peekl [_] a)
  (dg-peekr [_] d)
  (dg-popl [_] (digit3 b c d))
  (dg-popr [_] (digit3 a b c))
  (dg-measured-popl [_] (+ (-measured b) (-measured c) (-measured d)))
  (dg-measured-popr [_] (+ (-measured a) (-measured b) (-measured c)))
  (dg-split [_ pred-fn acc]
    (let [acc (+ acc (-measured a))]
      (if (pred-fn acc)
        [nil a (digit3 b c d)]
        (let [acc (+ acc (-measured b))]
          (if (pred-fn acc)
            [(digit1 a) b (digit2 c d)]
            (let [acc (+ acc (-measured c))]
              (if (pred-fn acc)
                [(digit2 a b) c (digit1 d)]
                [(digit3 a b c) d nil])))))))
  (dg-update [_ index f old-acc nest]
    (let [ma (-measured a)
          acc (+ old-acc ma)
          fx (fn [ml x] (if (zero? nest)
                          (f x)
                          (.dg-update x index f
                                      (+ old-acc ml) (dec nest))))]
      (if (> acc index)
        (digit4 (fx 0 a) b c d)
        (let [mb (-measured b)
              acc (+ acc mb)]
          (if (> acc index)
            (digit4 a (fx ma b) c d)
            (let [mc (-measured c)
                  acc (+ acc mc)]
              (if (> acc index)
                (digit4 a b (fx (+ ma mb) c) d)
                (digit4 a b c (fx (+ ma mb mc) d)))))))))
  (dg-map [_ f nest]
    (if (zero? nest)
      (digit4 (f a) (f b) (f c) (f d))
      (digit4 (.dg-map a f (dec nest))
              (.dg-map b f (dec nest))
              (.dg-map c f (dec nest))
              (.dg-map d f (dec nest)))))
  (dg-map-indexed [_ f index nest]
    (if (zero? nest)
      (digit4 (f index a) (f (inc index) b)
              (f (+ index 2) c) (f (+ index 3) d))
      (digit4 (.dg-map-indexed a f index (dec nest))
              (.dg-map-indexed b f (+ index (.dg-count a)) (dec nest))
              (.dg-map-indexed c f (+ index (.dg-count a)
                                      (.dg-count b)) (dec nest))
              (.dg-map-indexed d f (+ index (.dg-count a)
                                      (.dg-count b) (.dg-count c))
                               (dec nest)))))
  (dg-nth [_ index acc]
    (let [ma (-measured a)
          acc (+ acc ma)]
      (if (> acc index)
        [0 a]
        (let [mb (-measured b)
              acc (+ acc mb)]
          (if (> acc index)
            [ma b]
            (let [mc (-measured c)
                  acc (+ acc mc)]
              (if (> acc index)
                [(+ ma mb) c]
                [(+ ma mb mc) d])))))))
  (dg-reduce [_ pred-fn start nest]
    (if (zero? nest)
      (let [ra (pred-fn start a)]
        (if (reduced? ra)
          ra
          (let [rb (pred-fn ra b)]
            (if (reduced? rb)
              rb
              (let [rc (pred-fn rb c)]
                (if (reduced? rc)
                  rc
                  (pred-fn rc d)))))))
      (let [nest (dec nest)
            ra (.dg-reduce a pred-fn start nest)]
        (if (reduced? ra)
          ra
          (let [rb (.dg-reduce b pred-fn ra nest)]
            (if (reduced? rb)
              rb
              (let [rc (.dg-reduce c pred-fn rb nest)]
                (if (reduced? rc)
                  rc
                  (.dg-reduce d pred-fn rc nest)))))))))
  (dg-reduce-reverse [_ pred-fn start nest]
    (if (zero? nest)
      (let [rd (pred-fn start d)]
        (if (reduced? rd)
          rd
          (let [rc (pred-fn rd c)]
            (if (reduced? rc)
              rc
              (let [rb (pred-fn rc b)]
                (if (reduced? rb)
                  rb
                  (pred-fn rb a)))))))
      (let [nest (dec nest)
            rd (.dg-reduce-reverse d pred-fn start nest)]
        (if (reduced? rd)
          rd
          (let [rc (.dg-reduce-reverse c pred-fn rd nest)]
            (if (reduced? rc)
              rc
              (let [rb (.dg-reduce-reverse b pred-fn rc nest)]
                (if (reduced? rb)
                  rb
                  (.dg-reduce-reverse a pred-fn rb nest)))))))))
  (dg-reduce-kv [_ pred-fn start index nest]
    (if (zero? nest)
      (let [ra (pred-fn start index a)]
        (if (reduced? ra)
          ra
          (let [rb (pred-fn ra (inc index) b)]
            (if (reduced? rb)
              rb
              (let [rc (pred-fn rb (+ index 2) c)]
                (if (reduced? rc)
                  rc
                  (pred-fn rc (+ index 3) d)))))))
      (let [nest (dec nest)
            ra (.dg-reduce-kv a pred-fn start index nest)]
        (if (reduced? ra)
          ra
          (let [rb (.dg-reduce-kv b pred-fn ra (+ index (.dg-count a))
                                  nest)]
            (if (reduced? rb)
              rb
              (let [rc (.dg-reduce-kv c pred-fn rb
                                      (+ index (.dg-count a)
                                         (.dg-count b)) nest)]
                (if (reduced? rc)
                  rc
                  (.dg-reduce-kv d pred-fn rc
                                 (+ index (.dg-count a) (.dg-count b)
                                    (.dg-count c)) nest)))))))))
  (dg-reduce-kv-reverse [_ pred-fn start index nest]
    (if (zero? nest)
      (let [rd (pred-fn start index d)]
        (if (reduced? rd)
          rd
          (let [rc (pred-fn rd (dec index) c)]
            (if (reduced? rc)
              rc
              (let [rb (pred-fn rc (- index 2) b)]
                (if (reduced? rb)
                  rb
                  (pred-fn rb (- index 3) a)))))))
      (let [nest (dec nest)
            rd (.dg-reduce-kv-reverse d pred-fn start index nest)]
        (if (reduced? rd)
          rd
          (let [rc (.dg-reduce-kv-reverse c pred-fn rd
                                          (- index (.dg-count d))
                                          nest)]
            (if (reduced? rc)
              rc
              (let [rb (.dg-reduce-kv-reverse b pred-fn rc
                                              (- index (.dg-count d)
                                                 (.dg-count c)) nest)]
                (if (reduced? rb)
                  rb
                  (.dg-reduce-kv-reverse a pred-fn rb
                                         (- index
                                            (.dg-count d)
                                            (.dg-count c)
                                            (.dg-count b))
                                         nest)))))))))
  (dg-transient [_] (transient-digit 4 a b c d))
  (dg-tree [_] (deep-tree (digit2 a b) empty-tree (digit2 c d)))
  (dg-aconcat [_ arr]
    (ua/conj! arr a) (ua/conj! arr b)
    (ua/conj! arr c) (ua/conj! arr d) arr))

(defn- digit1
  "Returns new digit1 containing element a."
  [a]
  (Digit1. a (delay (-measured a))))

(defn- digit2
  "Returns new digit2 containing elements a and b."
  [a b]
  (Digit2. a b (delay (+ (-measured a) (-measured b)))))

(defn- digit3
  "Returns new digit3 containing elements a, b and c."
  [a b c]
  (Digit3. a b c
           (delay (+ (-measured a) (-measured b) (-measured c)))))

(defn- digit4
  "Returns new digit4 containing elements a, b, c and d."
  [a b c d]
  (Digit4. a b c d
           (delay (+ (-measured a) (-measured b)
                     (-measured c) (-measured d)))))

(defn- digit2-l
  "Returns new digit2 containing elements a and b, reusing
  measure of element b."
  [a b measure-ref]
  (Digit2. a b (delay (+ (-measured a) @measure-ref))))

(defn- digit2-r
  "Returns new digit2 containing elements a and b, reusing
  measure of element a."  
  [a b measure-ref]
  (Digit2. a b (delay (+ @measure-ref (-measured b)))))

(defn- digit3-l
  "Returns new digit3 containing elements a, b and c, reusing
  measures of elements b and c."  
  [a b c measure-ref]
  (Digit3. a b c (delay (+ (-measured a) @measure-ref))))

(defn- digit3-r
  "Returns new digit3 containing elements a, b and c, reusing
  measures of elements a and b."  
  [a b c measure-ref]
  (Digit3. a b c (delay (+ @measure-ref (-measured c)))))

(defn- digit4-l
  "Returns new digit4 containing elements a, b, c and d, reusing
  measures of elements b, c and d."  
  [a b c d measure-ref]
  (Digit4. a b c d (delay (+ (-measured a) @measure-ref))))

(defn- digit4-r
  "Returns new digit4 containing elements a, b, c and d, reusing
  measures of elements a, b and c."  
  [a b c d measure-ref]
  (Digit4. a b c d (delay (+ @measure-ref (-measured d)))))

;;; Trees

(defn- conjr-arr*
  "Returns new finger tree with elm-arr conjoined to the right."
  [t val-arr]
  (ua/reduce #(.ft-conjr %1 %2) t val-arr))

(defn- conjl-arr*
  "Returns new finger tree with elm-arr conjoined to the right.
  Order of elements is retained."
  [t val-arr]
  (ua/reduce-reverse #(.ft-conjl %1 %2) t val-arr))

(defn- conjr-arr-tr*
  "Returns new finger tree with elm-arr conjoined to the right.
  Uses transient finger tree to speed up the process."
  [t val-arr]
  (.ft-persistent (ua/reduce #(.ft-conjr %1 %2)
                             (.ft-transient t) val-arr)))

(defn- conjl-arr-tr*
  "Returns new finger tree with elm-arr conjoined to the right.
  Order of elements is retained.
  Uses transient finger tree to speed up the process."
  [t val-arr]
  (.ft-persistent (ua/reduce-reverse #(.ft-conjl %1 %2)
                                     (.ft-transient t) val-arr)))

(deftype EmptyTree []
  Object
  (toString [o]
    (pr-str o))

  ICounted
  (-count [_] 0)

  IEmptyableCollection
  (-empty [t] t)

  ICollection
  (-conj [t o] (single-tree o))

  IIndexed
  (-nth [_ _] (throw "nth: Empty finger tree!"))
  (-nth [_ _ not-found] not-found)

  ASeq
  ISeq
  (-first [_] nil)
  (-rest [_] (list))

  INext
  (-next [_] nil)

  ILookup
  (-lookup [_ _] nil)
  (-lookup [_ _ not-found] not-found)

  IAssociative
  (-contains-key? [_ _] nil)
  (-assoc [_ k v] (when (zero? k) (single-tree v)))

  IStack
  (-peek [_] nil)
  (-pop [_] nil)

  IVector
  (-assoc-n [_ n val] (when (zero? n) (single-tree val)))

  IReduce
  (-reduce [_ f] (f))
  (-reduce [_ _ start] start)

  IKVReduce
  (-kv-reduce [_ _ init] init)

  IEquiv
  (-equiv [t other] (clojure.core/empty? other))

  IHash
  (-hash [_] 0)

  ISeqable
  (-seq [_] nil)

  ISequential

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<EMPTY>")))

  IEditableCollection
  (-as-transient [_] (transient-empty-tree))

  IMeasured
  (-measured [_] 0)

  Object
  (ft-count [_] 0)
  (ft-depth [_] 0)
  (ft-conjl [_ o] (single-tree o))
  (ft-conjr [_ o] (single-tree o))
  (ft-peekl [_] nil)
  (ft-peekr [_] nil)
  (ft-popl [t] t)
  (ft-popr [t] t)
  (ft-measured-popl [_] 0)
  (ft-measured-popr [_] 0)
  (ft-app3 [_ ta t2] (ua/reduce-reverse #(.ft-conjl %1 %2) t2 ta))
  (ft-app3deep [_ ta t1] (ua/reduce #(.ft-conjr %1 %2) t1 ta))
  (ft-mape [t _ _] t)
  (ft-mape-indexed [t _ _ _] t)
  (ft-reduce [_ _ start _] start)
  (ft-reduce-reverse [_ _ start _] start)
  (ft-reduce-kv [_ _ start _ _] start)
  (ft-reduce-kv-reverse [_ _ start _ _] start)
  (ft-transient [_] (transient-empty-tree))
  (ft-splice [t index count elm-arr] (conjr-arr* t elm-arr)))

(defn- hash-coll* [coll]
  (clojure.core/reduce #(hash-combine %1 (hash %2 false))
                       (hash (first coll) false) (next coll)))

(deftype SingleTree [x]
  Object
  (toString [o]
    (pr-str o))

  ICounted
  (-count [_] 1)

  IEmptyableCollection
  (-empty [t] empty-tree)

  ICollection
  (-conj [t o] (deep-tree (digit1 x) empty-tree (digit1 o)))

  IIndexed
  (-nth [_ i] (if (zero? i) x (throw "nth: out of ftree bounds!")))
  (-nth [_ i not-found] (if (zero? i) x not-found))

  ASeq
  ISeq
  (-first [_] x)
  (-rest [_] (list))

  INext
  (-next [_] nil)

  ILookup
  (-lookup [_ i] (when (zero? i) x))
  (-lookup [_ i not-found] (if (zero? i) x not-found))

  IAssociative
  (-contains-key? [_ k] (zero? k))
  (-assoc [_ k v]
    (cond (zero? k) (single-tree v)
          (== k 1) (deep-tree (digit1 x) empty-tree (digit1 v))
          :else nil))
  
  IStack
  (-peek [_] x)
  (-pop [_] empty-tree)

  IVector
  (-assoc-n [_ n val]
        (cond (zero? n) (single-tree val)
          (== n 1) (deep-tree (digit1 x) empty-tree (digit1 val))
          :else nil))

  IReduce
  (-reduce [_ _] x)
  (-reduce [_ f start] (f start x))

  IKVReduce
  (-kv-reduce [_ f init] (f init x))

  IEquiv
  (-equiv [t other] (= (list x) other))

  IHash
  (-hash [t] (hash-coll* t))

  ISeqable
  (-seq [t] t)

  ISequential

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<SINGLE[" (pr-str (-measured x)) "] "
                    (pr-str x) ">")))

  IEditableCollection
  (-as-transient [_] (transient-single-tree x))

  IMeasured
  (-measured [_] (-measured x))

  Object
  (ft-count [_] (-measured x))
  (ft-depth [_] 1)
  (ft-conjl [_ o] (deep-tree (digit1 o) empty-tree (digit1 x)))
  (ft-conjr [_ o] (deep-tree (digit1 x) empty-tree (digit1 o)))
  (ft-peekl [_] x)
  (ft-peekr [_] x)
  (ft-popl [_] empty-tree)
  (ft-popr [_] empty-tree)
  (ft-measured-popl [_] 0)
  (ft-measured-popr [_] 0)
  (ft-split [_ _ _] [empty-tree x empty-tree])
  (ft-nth [_ _ _] [0 x])
  (ft-app3 [_ ta t2]
    (.ft-conjl
     (.ft-app3 empty-tree ta t2)
     x))
  (ft-app3deep [_ ta t1]
    (.ft-conjr (.ft-app3deep empty-tree ta t1) x))
  (ft-update [_ index f acc nest]
    (if (zero? nest)
      (single-tree (f x))
      (single-tree (.dg-update x index f acc (dec nest)))))
  (ft-mape [t f nest]
    (if (zero? nest)
      (single-tree (f x))
      (single-tree (.dg-map x f (dec nest)))))
  (ft-mape-indexed [t f index nest]
    (if (zero? nest)
      (single-tree (f index x))
      (single-tree (.dg-map-indexed x f index (dec nest)))))
  (ft-reduce [_ f start nest]
    (if (zero? nest)
      (f start x)
      (.dg-reduce x f start (dec nest))))
  (ft-reduce-reverse [_ f start nest]
    (if (zero? nest)
      (f start x)
      (.dg-reduce-reverse x f start (dec nest))))
  (ft-reduce-kv [_ f start index nest]
    (if (zero? nest)
      (f start index x)
      (.dg-reduce-kv x f start index (dec nest))))
  (ft-reduce-kv-reverse [_ f start index nest]
    (if (zero? nest)
      (f start index x)
      (.dg-reduce-kv-reverse x f start index (dec nest))))
  (ft-transient [_] (transient-single-tree x))
  (ft-splice [t index count elm-arr]
    (cond
     (and (zero? index)
          (or (pos? count)
              (nil? count)))
     (conjr-arr* empty-tree elm-arr)
     (zero? index)
     (.ft-conjr
      (conjr-arr* empty-tree elm-arr)
      x)
     :else
     (conjr-arr* t elm-arr))))

(defn- single-tree
  "Returns new single tree containing element x."
  [x]
  (SingleTree. x))

(deftype DelayedTree [tree-ref measure-val]
  Object
  (toString [o]
    (pr-str o))

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<DELAY[" (pr-str measure-val) "] "
                    (pr-str @tree-ref) " >")))
  IMeasured
  (-measured [_] measure-val)

  Object
  (ft-count [_] measure-val)
  (ft-depth [_] (.ft-depth @tree-ref))
  (ft-conjl [_ o] (.ft-conjl @tree-ref o))
  (ft-conjr [_ o] (.ft-conjr @tree-ref o))
  (ft-peekl [_] (.ft-peekl @tree-ref))
  (ft-peekr [_] (.ft-peekr @tree-ref))
  (ft-popl [_] (.ft-popl @tree-ref))
  (ft-popr [_] (.ft-popr @tree-ref))
  (ft-measured-popl [_] (.ft-measured-popl @tree-ref))
  (ft-measured-popr [_] (.ft-measured-popr @tree-ref))
  (ft-mape [_ f nest] (.ft-mape @tree-ref f nest))
  (ft-mape-indexed [_ f index nest]
    (.ft-mape-indexed @tree-ref f index nest))
  (ft-reduce [_ f start nest] (.ft-reduce @tree-ref f start nest))
  (ft-reduce-reverse [_ f start nest]
    (.ft-reduce-reverse @tree-ref f start nest))
  (ft-reduce-kv [_ f start index nest]
    (.ft-reduce-kv @tree-ref f start index nest))
  (ft-reduce-kv-reverse [_ f start index nest]
    (.ft-reduce-kv-reverse @tree-ref f start index nest))
  (ft-nth [_ index acc] (.ft-nth @tree-ref index acc))
  (ft-update [_ index f acc nest]
    (.ft-update @tree-ref index f acc nest))
  (ft-split [_ pred acc] (.ft-split @tree-ref pred acc))
  (ft-app3 [_ ta t2] (.ft-app3 @tree-ref ta t2))
  (ft-app3deep [_ ta t1] (.ft-app3deep @tree-ref ta t1))
  (ft-splice [_ index count elm-arr]
    (.ft-splice @tree-ref index count elm-arr))
  (ft-transient [_] (.ft-transient @tree-ref)))

(defn- deep-left
  "Creates deep finger tree from pre mid suf,
  where pre (and mid) is possibly an empty tree."
  [pre mid suf]
  (cond
   (not (nil? pre)) (deep-tree pre mid suf)
   (or (instance? EmptyTree mid)
       (and (instance? DelayedTree mid) (zero? (.ft-count mid))))
   (.dg-tree suf)
   :else (deep-tree (.ft-peekl mid)
                    (ftm/delayed-tree
                     (.ft-popl mid)
                     (.ft-measured-popl mid))
                    suf)))

(defn- deep-right
  "Creates deep finger tree from pre mid suf,
  where seq (and mid) is possibly an empty tree."
  [pre mid suf]
  (cond
   (not (nil? suf)) (deep-tree pre mid suf)
   (or (instance? EmptyTree mid)
       (and (instance? DelayedTree mid) (zero? (.ft-count mid))))
   (.dg-tree pre)
   :else (deep-tree pre
                    (ftm/delayed-tree
                     (.ft-popr mid)
                     (.ft-measured-popr mid))
                    (.ft-peekr mid))))

(defn- nodes
  "Returns array of digits created from array of elements."
  [xa]
  (let [arr (ua/empty)
        l (ua/count xa)]
    (loop [i 0]
      (condp == (- l i)
        2 (ua/conj! arr (digit2 (ua/nth-unchecked xa i)
                                 (ua/nth-unchecked xa (inc i))))
        3 (ua/conj! arr (digit3 (ua/nth-unchecked xa i)
                                 (ua/nth-unchecked xa (inc i))
                                 (ua/nth-unchecked xa (+ i 2))))
        4 (do (ua/conj! arr (digit2 (ua/nth-unchecked xa i)
                                     (ua/nth-unchecked xa (inc i))))
              (ua/conj! arr (digit2 (ua/nth-unchecked xa (+ i 2))
                                     (ua/nth-unchecked xa (+ i 3)))))
        (do (ua/conj! arr (digit3 (ua/nth-unchecked xa i)
                                   (ua/nth-unchecked xa (inc i))
                                   (ua/nth-unchecked xa (+ i 2))))
            (recur (+ i 3)))))
    arr))

(defn- equiv-sequential*
  "Assumes x is sequential. Returns true if x equals y, otherwise
  returns false."
  [x y]
  (boolean
   (when (sequential? y)
     (loop [xs (seq x) ys (seq y)]
       (cond (nil? xs) (nil? ys)
             (nil? ys) false
             (= (first xs) (first ys)) (recur (next xs) (next ys))
             :else false)))))

(declare nth nth* assoc reduce reduce-kv)

(deftype DeepTree [pre mid suf measure-ref]
  Object
  (toString [o]
    (pr-str o))

  ICounted
  (-count [_] @measure-ref)

  IEmptyableCollection
  (-empty [t] empty-tree)

  ICollection
  (-conj [t o] (.ft-conjr t o))

  IIndexed
  (-nth [t i] (nth t i))
  (-nth [t i not-found] (nth* t i not-found))

  ASeq
  ISeq
  (-first [t] (.ft-peekl t))
  (-rest [t] (.ft-popl t))

  INext
  (-next [t] (.ft-popl t))

  ILookup
  (-lookup [t i] (nth* t i nil))
  (-lookup [t i not-found] (nth* t i not-found))

  IAssociative
  (-contains-key? [_ k] (< -1 k @measure-ref))
  (-assoc [t k v] (assoc t k v))
  
  IStack
  (-peek [t] (.ft-peekr t))
  (-pop [t] (.ft-popr t))

  IVector
  (-assoc-n [t n val] (assoc t n val))

  IReduce
  (-reduce [t f] (reduce f (.ft-peekl t) (.ft-popl t)))
  (-reduce [t f start] (reduce f start t))

  IKVReduce
  (-kv-reduce [t f init] (reduce-kv f init t))

  IEquiv
  (-equiv [t other] (equiv-sequential* t other))

  IHash
  (-hash [t] (hash-coll* t))

  ISeqable
  (-seq [t] t)

  ISequential

  IPrintWithWriter
  (-pr-writer [_ wr _]
    (-write wr (str "#<DEEP[" (pr-str @measure-ref) "] "
                    (pr-str pre) " " (pr-str mid) " "
                    (pr-str suf) ">")))

  IEditableCollection
  (-as-transient [t] (.ft-transient t))

  IMeasured
  (-measured [_] @measure-ref)

  Object
  (ft-count [_] @measure-ref)
  (ft-depth [_] (inc (.ft-depth mid)))
  (ft-conjl [_ elm]
    (if-not (dg-4? pre)
      (deep-tree (.dg-conjl pre elm) mid suf)
      (let [new-digit (digit3 (.-b pre) (.-c pre) (.-d pre))]
        (deep-tree (digit2 elm (.-a pre))
                   (.ft-conjl mid new-digit)
                   suf))))
  (ft-conjr [_ elm]
    (if-not (dg-4? suf)
      (deep-tree pre mid (.dg-conjr suf elm))
      (let [new-digit (digit3 (.-a suf) (.-b suf) (.-c suf))]
        (deep-tree pre
                   (.ft-conjr mid new-digit)
                   (digit2 (.-d suf) elm)))))
  (ft-peekl [_] (.dg-peekl pre))
  (ft-peekr [_] (.dg-peekr suf))
  (ft-popl [_] (deep-left (.dg-popl pre) mid suf))
  (ft-popr [_] (deep-right pre mid (.dg-popr suf)))
  (ft-measured-popl [_] (+ (.dg-measured-popl pre)
                           (-measured mid)
                           (-measured suf)))
  (ft-measured-popr [_] (+ (-measured pre)
                           (-measured mid)
                           (.dg-measured-popr suf)))
  (ft-mape [t map-fn nest]
    (deep-tree (.dg-map pre map-fn nest)
               (.ft-mape mid map-fn (inc nest))
               (.dg-map suf map-fn nest)))
  (ft-mape-indexed [t map-fn index nest]
    (deep-tree (.dg-map-indexed pre map-fn index nest)
               (.ft-mape-indexed mid map-fn
                                (+ index (.dg-count pre)) (inc nest))
               (.dg-map-indexed suf map-fn
                                (+ index (.dg-count pre)
                                   (.ft-count mid)) nest)))
  (ft-reduce [_ reduce-fn start nest]
    (let [rpre (.dg-reduce pre reduce-fn start nest)]
      (if (reduced? rpre)
        rpre
        (let [rmid (.ft-reduce mid reduce-fn rpre (inc nest))]
          (if (reduced? rmid)
            rmid
            (.dg-reduce suf reduce-fn rmid nest))))))
  (ft-reduce-reverse [_ reduce-fn start nest]
    (let [rsuf (.dg-reduce-reverse suf reduce-fn start nest)]
      (if (reduced? rsuf)
        rsuf
        (let [rmid (.ft-reduce-reverse mid reduce-fn rsuf (inc nest))]
          (if (reduced? rmid)
            rmid
            (.dg-reduce-reverse pre reduce-fn rmid nest))))))
  (ft-reduce-kv [_ reduce-fn start index nest]
    (let [rpre (.dg-reduce-kv pre reduce-fn start index nest)]
      (if (reduced? rpre)
        rpre
        (let [rmid (.ft-reduce-kv mid reduce-fn rpre
                                  (+ index (.dg-count pre))
                                  (inc nest))]
          (if (reduced? rmid)
            rmid
            (.dg-reduce-kv suf reduce-fn rmid
                           (+ index (.dg-count pre) (.ft-count mid))
                           nest))))))
  (ft-reduce-kv-reverse [_ reduce-fn start index nest]
    (let [rsuf (.dg-reduce-kv-reverse suf reduce-fn start index nest)]
      (if (reduced? rsuf)
        rsuf
        (let [rmid (.ft-reduce-kv-reverse mid reduce-fn rsuf
                                          (- index (.dg-count suf))
                                          (inc nest))]
          (if (reduced? rmid)
            rmid
            (.dg-reduce-kv-reverse pre reduce-fn rmid
                                   (- index (.dg-count suf)
                                      (.ft-count mid)) nest))))))
  (ft-nth [_ index acc]
    (let [mpre (-measured pre)
          vpr (+ acc mpre)]
      (if (> vpr index)
        ;; nth in pred
        (.dg-nth pre index acc)
        (let [mmid (-measured mid)
              vm (+ vpr mmid)]
          (if (> vm index)
            ;; nth in mid
            (let [[ml xs] ^ArrayVector (.ft-nth mid index vpr)
                  [sl sx] ^ArrayVector (.dg-nth xs index (+ vpr ml))]
              [(+ mpre ml sl) sx])
            ;; nth in suf
            (let [[sl sx] ^ArrayVector (.dg-nth suf index vm)]
              [(+ mpre mmid sl) sx]))))))
  (ft-update [_ index update-fn acc nest]
    (let [mpre (-measured pre)
          vpr (+ acc mpre)]
      (if (> vpr index)
        ;; update in pred
        (deep-tree (.dg-update pre index update-fn acc nest) mid suf)
        (let [mmid (-measured mid)
              vm (+ vpr mmid)]
          (if (> vm index)
            ;; update in mid
            (deep-tree pre
                       (.ft-update mid index update-fn
                                   (+ acc mpre) (inc nest))
                       suf)
            ;; update in suf
            (deep-tree pre mid
                       (.dg-update suf index update-fn
                                   (+ acc mpre mmid) nest)))))))
  (ft-split [_ pred-fn acc]
    ;; TODO: use transients?
    (let [vpr (+ acc (-measured pre))]
      (if (pred-fn vpr)
        ;; split in pre
        (let [[sl sx sr] ^ArrayVector (.dg-split pre pred-fn acc)
              sl-tree (if (nil? sl) empty-tree (.dg-tree sl))]
          [sl-tree sx (deep-left sr mid suf)])
        (let [vm (+ vpr (-measured mid))]
          (if (pred-fn vm)
            ;; split in mid
            (let [[ml xs mr] ^ArrayVector (.ft-split mid pred-fn vpr)
                  [sl sx sr] ^ArrayVector
                  (.dg-split xs pred-fn (+ vpr (-measured ml)))]
              [(deep-right pre ml sl) sx (deep-left sr mr suf)])
            ;; split in suf
            (let [[sl sx sr] ^ArrayVector (.dg-split suf pred-fn vm)
                  sr-tree (if (nil? sr) empty-tree (.dg-tree sr))]
              [(deep-right pre mid sl) sx sr-tree]))))))  
  (ft-app3 [t1 ta t2] (.ft-app3deep t2 ta t1))
  (ft-app3deep [_ ta t1]
    (deep-tree
     (.-pre t1)
     (.ft-app3 (.-mid t1)
               (nodes
                (let [arr (array)
                      arr (.dg-aconcat (.-suf t1) arr)
                      arr (if (nil? ta) arr (ua/cat arr ta))
                      arr (.dg-aconcat pre arr)]
                  arr))
               mid)
     suf))
  (ft-splice [t index count elm-arr]
    (let [[before x after] ^ArrayVector (.ft-split t #(> % index) 0)]
      (cond
       (zero? count)
       (.ft-app3 before elm-arr (.ft-conjl after x))
       (or (nil? count) (instance? EmptyTree after)
           (and (instance? DelayedTree after)
                (zero? (.ft-count after))))
       (let [new-t (if (>= index (-measured t)) t before)]
         (if (nil? elm-arr)
           new-t
           (conjr-arr* new-t elm-arr)))
       (== count 1) (.ft-app3 before elm-arr after)
       :else (let [[_ _ after] ^ArrayVector
                   (.ft-split after #(> % (- count 2)) 0)]
               (.ft-app3 before elm-arr after)))))
  (ft-transient [_] (transient-deep-tree
                     (.dg-transient pre)
                     (.ft-transient mid)
                     (.dg-transient suf))))

(defn- deep-tree-wmv
  "Creates new deep tree.
  Returns created tree."
  [pre mid suf measure-val]
  (DeepTree. pre mid suf measure-val))

(defn- deep-tree
  "Creates new deep tree.
  Returns created tree."
  [pre mid suf]
  (deep-tree-wmv
   pre mid suf
   (delay (+ (-measured pre) (-measured mid) (-measured suf)))))

;;; Transients

(def transient-threshold 11)

(deftype TransientDigit [^:mutable l ^:mutable a ^:mutable b
                         ^:mutable c ^:mutable d]
  Object
  (dg-conjl [d o]
    (set! (.-l d) (inc (.-l d)))
    (condp == l
      2 (do (set! (.-b d) (.-a d))
            (set! (.-a d) o))
      3 (do (set! (.-c d) (.-b d))
            (set! (.-b d) (.-a d))
            (set! (.-a d) o))
      (do (set! (.-d d) (.-c d))
          (set! (.-c d) (.-b d))
          (set! (.-b d) (.-a d))
          (set! (.-a d) o)))
    d)
  (dg-conjr [d o]
    (set! (.-l d) (inc (.-l d)))
    (condp == l
      2 (set! (.-b d) o)
      3 (set! (.-c d) o)
      (set! (.-d d) o))
    d)
  (dg-digit2 [d a b]
    (set! (.-l d) 2)
    (set! (.-a d) a)
    (set! (.-b d) b)
    d)
  (dg-persistent [_]
    (condp == l
      1 (digit1 a)
      2 (digit2 a b)
      3 (digit3 a b c)
      (digit4 a b c d))))

(deftype TransientEmptySingleTree [^:mutable x]
  ITransientCollection
  (-conj! [t v] (.ft-conjr t v))
  (-persistent! [t] (.ft-persistent t))

  Object
  (ft-conjl [t o]
    (if (nil? x)
      (do (set! (.-x t) o)
          t)
      (let [old-x x]
        (set! (.-x t) nil)
        (transient-deep-tree
         (transient-digit 1 o nil nil nil)
         t
         (transient-digit 1 old-x nil nil nil)))))
  (ft-conjr [t o]
    (if (nil? x)
      (do (set! (.-x t) o)
          t)
      (let [old-x x]
        (set! (.-x t) nil)
        (transient-deep-tree
         (transient-digit 1 old-x nil nil nil)
         t
         (transient-digit 1 o nil nil nil)))))
  (ft-persistent [_]
    (if (nil? x)
      empty-tree
      (single-tree x))))

(deftype TransientDeepTree [^:mutable pre ^:mutable mid ^:mutable suf]
  ITransientCollection
  (-conj! [t v] (.ft-conjr t v))
  (-persistent! [t] (.ft-persistent t))

  Object
  (ft-conjl [t o]
    (if-not (dg-4? pre)
      (.dg-conjl (.-pre t) o)
      (let [new-digit (digit3 (.-b pre) (.-c pre) (.-d pre))]
        (set! (.-mid t) (.ft-conjl (.-mid t) new-digit))
        (.dg-digit2 (.-pre t) o (.-a pre))))
    t)
  (ft-conjr [t o]
    (if-not (dg-4? suf)
      (.dg-conjr (.-suf t) o)
      (let [new-digit (digit3 (.-a suf) (.-b suf) (.-c suf))]
        (set! (.-mid t) (.ft-conjr (.-mid t) new-digit))
        (.dg-digit2 (.-suf t) (.-d suf) o)))
    t)
  (ft-persistent [_]
    (deep-tree (.dg-persistent pre)
               (.ft-persistent mid)
               (.dg-persistent suf))))

(defn- transient-digit
  "Returns new transient digit."
  [l a b c d]
  (TransientDigit. l a b c d))

(defn- transient-empty-tree
  "Returns new transient empty tree."
  []
  (TransientEmptySingleTree. nil))

(defn- transient-single-tree
  "Returns new transient single tree."
  [x]
  (TransientEmptySingleTree. x))

(defn- transient-deep-tree
  "Returns new transient deep tree."
  [pre mid suf]
  (TransientDeepTree. pre mid suf))

(defn ^boolean dg-4?
  "Returns true if digit contains 4 items."
  [digit]
  (or (instance? Digit4 digit)
      (and (instance? TransientDigit digit)
           (== (.-l digit) 4))))

(def empty-tree (EmptyTree.))

;;;; Public API

;;; Creation

(defn empty
  "Returns empty finger tree collection."
  []
  empty-tree)

;;; Access

(defn ^boolean empty?
  "Returns true if a is empty tree or nil.
  Faster variant of clojure.core/empty?."
  [t]
  (or (instance? EmptyTree t)
      (and (instance? DelayedTree t)
           (zero? (.ft-count t)))))

(defn count
  "Returns the number of items in the tree.
  (count nil) returns 0. Faster variant of clojure.core/count."
  [t]
  (if (nil? t)
    0
    (.ft-count t)))

(defn nth
  "Returns the value at the index. nth throws an exception if index
  out of bounds. Faster variant of clojure.core/nth"
  [t index]
  (when-not (nil? t)
    (if (or (neg? index) (<= (.ft-count t) index))
      (throw "nth: Index out of bounds!")
      (let [[_ val] ^ArrayVector (.ft-nth t index 0)]
        val))))

(defn nth*
  "Returns the value at the index or not-found, if index
  out of bounds. Faster variant of clojure.core/nth"
  [t index not-found]
  (if (or (nil? t) (neg? index) (<= (.ft-count t) index))
    not-found
    (let [[_ val] ^ArrayVector (.ft-nth t index 0)]
    val)))

(defn nth-unchecked
  "Returns the value at the index. Does not check for boundaries.
  Faster variant of clojure.core/nth."
  [t index]
  (let [[_ val] ^ArrayVector (.ft-nth t index 0)]
    val))

(defn peekl
  "Same as, but much more efficient than, first.
  If the tree is empty, returns nil."
  [t]
  (when-not (empty? t)
    (.ft-peekl t)))

(defn peekr
  "Same as, but much more efficient than, last.
  If the tree is empty, returns nil."
  [t]
  (when-not (empty? t)
    (.ft-peekr t)))

(def peek peekr)

(defn peekl-unchecked
  "Same as, but much more efficient than, first.
  Undefined behavior if the tree is empty."
  [t]
  (.ft-peekl t))

(defn peekr-unchecked
  "Same as, but much more efficient than, last.
  Undefined behavior if the tree is empty."
  [t]
  (.ft-peekr t))

(def peek-unchecked peekr-unchecked)

;;; Modify

(declare trimr triml)

(defn slice
  "Returns the subtree of t beginning at start inclusive, and ending
  at end, exclusive. start must be less than end."
  [t start end]
  (let [new-end (- (.ft-count t) end start)]
    (-> t
        (triml start)
        (trimr new-end))))

(defn slice-from
  "Returns the subtree of t beginning at start inclusive, until the
  end of tree."
  [t start]
  (triml t start))

(defn slice-to
  "Returns the subarray of t from its start, and ending
  at end, exclusive."
  [t end]
  (cond (>= end (.ft-count t)) t
        (pos? end) (nth (.ft-split t #(> % end) 0) 0)
        :else empty-tree))

(defn split-at
  "Returns vector of trees produced by splitting t at index pos."
  [t index]
  (if (< index (.ft-count t))
    (let [[t1 x t2] ^ArrayVector (.ft-split t #(> % index) 0)]
      [t1 (.ft-conjl t2 x)])
    [t empty-tree]))

(defn cat
  "Returns new tree which is a concatenation of t1 and t2.
  Eager version of clojure.core/concat."
  [t1 t2]
  (.ft-app3 t1 nil t2))

(defn popl
  "Returns new tree without first item. If the tree is empty,
  throws an exception."
  [t]
  (if (empty? t)
    (throw "Cannot pop empty tree or nil.")
    (.ft-popl t)))

(defn popr
  "Returns new tree without last item. If the tree is empty,
  throws an exception."
  [t]
  (if (empty? t)
    (throw "Cannot pop empty tree or nil.")
    (.ft-popr t)))

(def pop popr)

(defn popl-unchecked
  "Returns new tree without first item.
  Undefined behavior if the tree is empty."
  [t]
  (.ft-popl t))

(defn popr-unchecked
  "Returns new tree without last item.
  Undefined behavior if the tree is empty."
  [t]
  (.ft-popr t))

(def pop-unchecked popr-unchecked)

(defn assoc
  "Assoc[iate]. Returns a new tree that contains val at index.
  Note - index must be <= (count tree)."
  [t index val]
  (if (< index (.ft-count t))
    (.ft-update t index (fn [_] val) 0 0)
    (.ft-conjr t val)))

(defn update
  "Returns a new tree that contains updated val at index.
  Note - index must be < (count tree)."
  [t index f]
  (.ft-update t index f 0 0))

(defn conjl
  "conj[oin]. Returns a new tree with var added to the left."
  [t val]
  (.ft-conjl t val))

(defn conjr
  "conj[oin]. Returns a new tree with var added to the right."
  [t val]
  (.ft-conjr t val))

(def conj conjr)

(defn conjl-arr
  "conj[oin]. Returns a new tree with var-arr added to the left.
  Order of elements is retained."
  [t val-arr]
  (if (or (instance? EmptyTree t)
          (instance? SingleTree t)
          (and (instance? DelayedTree t) (< (.ft-count t) 2))
          (> (.-length val-arr) transient-threshold))
    (conjl-arr-tr* t val-arr)
    (conjl-arr* t val-arr)))

(defn conjr-arr
  "conj[oin]. Returns a new tree with var-arr added to the right."
  [t val-arr]
  (if (or (instance? EmptyTree t)
          (instance? SingleTree t)
          (and (instance? DelayedTree t) (< (.ft-count t) 2))         
          (> (.-length val-arr) transient-threshold))
    (conjr-arr-tr* t val-arr)
    (conjr-arr* t val-arr)))

(def conj-arr conjr-arr)

(defn splice
  "Returns new tree with n items starting at index pos
  replaced with val."
  [t index n val]
  (.ft-splice t index n (array val)))

(defn splice-arr
  "Returns new tree with n items starting at index pos
  replaced with items in val-arr."
  [t index n val-arr]
  (.ft-splice t index n val-arr))

(defn insert-before
  "Returns a new tree with val inserted before index."
  [t index val]
  (if (< index (.ft-count t))
    (let [[dl x dr] ^ArrayVector (.ft-split t #(> % index) 0)]
      (.ft-app3 dl (array val x) dr))
    (.ft-conjr t val)))

(defn insert-before-arr
  "Returns a new tree with val-arr inserted before index."
  [t index val-arr]
  (if (< index (.ft-count t))
    (let [[dl x dr] ^ArrayVector (.ft-split t #(> % index) 0)]
      (.ft-app3 dl val-arr (.ft-conjl dr x)))
    (conjr-arr t val-arr)))

(defn remove-at
  "Returns new tree with item at index position removed."
  [t index]
  (.ft-splice t index 1 nil))

(defn remove-n
  "Returns new tree with n items starting at index position
  (index position included) removed."
  [t index n]
  (.ft-splice t index n nil))

(defn rip
  "Returns result of ripping tree at index pos.
  Returns [pre-tree item suf-tree]. Throws exception for empty tree."
  [t index]
  (if (< index (.ft-count t))
    (.ft-split t #(> % index) 0)
    [t nil empty-tree]))

(defn sew
  "Returns sewed tree. Kinda opposite of rip."
  [pre-tree item-arr suf-tree]
  (.ft-app3 pre-tree item-arr suf-tree))

(defn triml
  "Returns new tree with n items trimmed from left."
  [t n]
  (cond (>= n (.ft-count t)) empty-tree
        (pos? n) (nth (.ft-split t #(> % (dec n)) 0) 2)
        :else t))

(defn trimr
  "Returns new tree with n items trimmed from right."
  [t n]
  (cond (>= n (.ft-count t)) empty-tree
        (pos? n) (nth (.ft-split t #(> % (- (.ft-count t) n)) 0) 0)
        :else t))

(defn trim
  "Returns new tree with nl items trimmed from left and nr items
  trimmed from right."
  [t nl nr]
  (-> t (triml nl) (trimr nr)))

;;; Use

(defn mape
  "Eager ftree map. Returns new tree.
  f takes element as an argument.
  Eager version of clojure.core/map."
  [f t]
  (.ft-mape t f 0))

(defn mape-indexed
  "Eager tree map-indexed. Returns new tree.
  f takes two arguments, index and element.
  Eager version of clojure.core/map-indexed."
  [f t]
  (.ft-mape-indexed t f 0 0))

(defn reduce
  "Tree reduce. Faster variant of clojure.core/reduce."
  [f init t]
  (if (nil? t)
    init
    (let [r (.ft-reduce t f init 0)]
      (if (reduced? r) @r r))))

(defn reduce-reverse
  "Reverse tree reduce. Very fast."
  [f init t]
  (if (nil? t)
    init
    (let [r (.ft-reduce-reverse t f init 0)]
      (if (reduced? r) @r r))))

(defn reduce-kv
  "Tree reduce-kv. Faster variant of clojure.core/reduce-kv."
  [f init t]
  (if (nil? t)
    init
    (let [r (.ft-reduce-kv t f init 0 0)]
      (if (reduced? r) @r r))))

(defn reduce-kv-reverse
  "Reverse tree reduce-kv. Very fast."
  [f init t]
  (if (nil? t)
    init
    (let [r (.ft-reduce-kv-reverse t f init (dec (.ft-count t)) 0)]
      (if (reduced? r) @r r))))

(defn index-of
  "Returns index of val inside t.
  Returns -1 if val was not found."
  [t val]
  (let [f (fn [_ index x] (if (identical? val x) (reduced index) -1))]
    (reduce-kv f -1 t)))

(defn index-of-from
  "Returns index of val inside t, starting at
  index-from. Returns -1 if val was not found."
  [t val index-from]
  (let [t (slice-from t index-from)
        f (fn [_ index x] (if (identical? val x)
                            (reduced (+ index index-from))
                            -1))]
    (reduce-kv f -1 t)))
