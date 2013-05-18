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

(ns wagjo.data.char
  "Char manipulation."
  (:refer-clojure :exclude [char]))

;;;; Public API

(defn char-code
  "Returns char code of a character c."
  [c]
  (.charCodeAt c 0))

(defn char
  "Returns character for a given code."
  [code]
  (.fromCharCode js/String code))

(defn ^boolean printable-code?
  "Returns true if code is printable.
  Note that this function does not consider CR/LF to be printable."
  [code]
  (> code 31))

(defn ^boolean tab-code?
  "Returns true if code represents a TAB."
  [code]
  (== code 9))

(defn ^boolean newline-code?
  "Returns true if code represents a newline."
  [code]
  (== code 10))

(defn ^boolean space-code?
  "Returns true if code represents a space."
  [code]
  (== code 32))
