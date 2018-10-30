;;;; package.lisp

(defpackage #:space-invaders
  (:use #:cl #:cl-arrows)
  (:import-from :alexandria
                :when-let
                :if-let
                :iota
                :map-iota
                :curry
                :xor))
