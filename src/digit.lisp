;;;; digit.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defconstant $base
  (1+ most-positive-fixnum)
  ;;(expt 2 64)
  )
(defconstant $digit-bits
  (1- (integer-length $base))
  ;;64
  )

(deftype digit ()
  "A digit in an MPZ."
  `(integer 0 (,$base)))

(deftype intermediate ()
  "An intermediate computation with a digit."
  `(signed-byte ,(+ 2 $digit-bits)))

(defmacro define-fx-op (op-name (base-op &rest args))
  `(progn
     (declaim (inline ,op-name))
     (defun ,op-name ,args
       (declare (type digit ,@args)
                (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
       (the digit (mod (,base-op ,@args) ,$base)))))

(define-fx-op fx+ (+ a b))
(define-fx-op fx- (- a b))
(define-fx-op fx1+ (1+ a))
(define-fx-op fx1- (1- a))
(define-fx-op fxneg (- a))
(define-fx-op fx* (* a b))
(define-fx-op fx/ (floor a b))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                mul128))
(defun mul128 (x y)
  #+sbcl (mul128 x y)
  #-sbcl (let ((r (* x y)))
           (ldb (byte 64 0) r)
           (ldb (byte 64 64) r)))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                div128))
(defun div128 (dividend-lo dividend-hi divisor)
  #+sbcl (div128 dividend-lo dividend-hi divisor)
  #-sbcl (truncate (dpb dividend-hi (byte 64 64) dividend-lo) dividend))

