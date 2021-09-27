;;;; errors.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(define-condition error-invalid-op (simple-error)
  ())

(define-condition error-division-by-zero (simple-error)
  ())

(define-condition error-overflow (simple-error)
  ())

(define-condition error-underflow (simple-error)
  ())

(define-condition error-inexact (simple-error)
  ())

