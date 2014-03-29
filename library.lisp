;;;; library.lisp

(in-package #:libsass)

(define-foreign-library libsass
  (:unix (:or "libsass.so.0" "libsass.so"))
  (t (:default "libsass")))

(use-foreign-library libsass)
