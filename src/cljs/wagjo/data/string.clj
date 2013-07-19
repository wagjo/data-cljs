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
  "String manipulation.")

(defmacro cat
  "Macro for string concatenation."
  [& body]
  (condp == (count body)
    1 (first body)
    2 (cons 'wagjo.data.string/cat2 body)
    3 (cons 'wagjo.data.string/cat3 body)
    4 (cons 'wagjo.data.string/cat4 body)
    5 (cons 'wagjo.data.string/cat5 body)
    6 (cons 'wagjo.data.string/cat6 body)
    7 (cons 'wagjo.data.string/cat7 body)
    8 (cons 'wagjo.data.string/cat8 body)
    9 (cons 'wagjo.data.string/cat9 body)
    10 (cons 'wagjo.data.string/cat10 body)
    (list 'wagjo.data.string/cat-seq (vec body))))

(comment

  (macroexpand-1 '(cat "ahoj"))

  (macroexpand-1 '(cat "ahoj" "jozo"))

  (macroexpand-1 '(cat "ahoj" "si" "super"))

  (macroexpand-1 '(cat "ahoj" "2" "3" "4"))

  (macroexpand-1 '(cat "ahoj" "2" "3" "4" "5"))

  (macroexpand-1 '(cat "ahoj" "2" "3" "4" "5" "6"))

  (macroexpand-1 '(cat "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))

  (macroexpand-1 `(cat ~@(repeat 15 "s")))

)
