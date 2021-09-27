;;;; rounding.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

;;; Rounding

(define-symbolic-enumeration rounding-mode
  rnd/n                                 ; Round to nearest, ties with even.
  rnd/z                                 ; Round toward zero.
  rnd/d                                 ; Round down, toward -inf.
  rnd/u                                 ; Round up, toward +inf.
  rnd/na                                ; Round to nearest, ties away from zero.
  rnd/a                                 ; Round away from zero.
  rnd/f)                                ; Faithful rounding


;;; Precision

(defconstant $min-precision 2)
(defconstant $max-precision (- (expt 2 (- $digit-bits 2)) 2))
(defconstant $inf-precision (1+ $max-precision))
