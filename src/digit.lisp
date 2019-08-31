;;;; digit.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defconstant $digit-bits 64)

(defconstant $base (expt 2 $digit-bits))

;; These two have the same value, but are used differently. The former
;; is a numerical quantity. The latter is for bit twiddling.
(defconstant $max-digit (1- $base))
(defconstant $digit-ones (ldb (byte $digit-bits 0) -1))

(deftype digit ()
  "A digit in an MPZ."
  `(unsigned-byte ,$digit-bits))

(defmacro define-fx-op (op-name (base-op &rest args))
  `(progn
     (declaim (inline ,op-name))
     (defun ,op-name ,args
       (declare (type digit ,@args)
                (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
       (the digit (ldb (byte $digit-bits 0) (,base-op ,@args))))))

(define-fx-op fx+ (+ a b))
(define-fx-op fx- (- a b))
(define-fx-op fx1+ (1+ a))
(define-fx-op fx1- (1- a))
(define-fx-op fxneg (- a))
(define-fx-op fx* (* a b))
(define-fx-op fx/ (floor a b))

;; 64 x 64 -> 128
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) bit &optional))
                add64))
(defun add64 (x y)
  #+sbcl (add64 x y)
  #-sbcl (let ((s (+ x y)))
           (values (ldb (byte 64 0) s)
                   (ldb (byte 1 64) s))))

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

(declaim (inline complement-digit))
(defun complement-digit (n)
  (declare (type digit n))
  (logxor n $digit-ones))
