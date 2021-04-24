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

(declaim (inline bytes-for-digits))
(defun bytes-for-digits (num-digits)
  (ceiling (* $digit-bits num-digits) 8))

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

(declaim (ftype (function ((unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                ub64/2))
(defun ub64/2 (x)
  #+hypergeometrica-intrinsics
  (ub64/2 x)
  #-hypergeometrica-intrinsics
  (ash x -1))

;; 64 x 64 -> 128
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) bit &optional))
                add64))
(defun add64 (x y)
  #+hypergeometrica-intrinsics
  (add64 x y)
  #-hypergeometrica-intrinsics
  (let ((s (+ x y)))
    (values (ldb (byte 64 0) s)
            (ldb (byte 1 64) s))))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                mul128))
(defun mul128 (x y)
  #+hypergeometrica-intrinsics
  (mul128 x y)
  #-hypergeometrica-intrinsics
  (let ((r (* x y)))
    (values (ldb (byte 64 0) r)
            (ldb (byte 64 64) r))))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                div128))
(defun div128 (dividend-lo dividend-hi divisor)
  #+hypergeometrica-intrinsics
  (div128 dividend-lo dividend-hi divisor)
  #-hypergeometrica-intrinsics
  (truncate (dpb dividend-hi (byte 64 64) dividend-lo) divisor))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                add128 sub128))
(defun add128 (alo ahi blo bhi)
  "Compute

    (alo + ahi*2^64) + (blo + bhi*2^64)
"
  #+hypergeometrica-intrinsics
  (add128 alo ahi blo bhi)
  #-hypergeometrica-intrinsics
  (let ((sum (+ alo blo (* (expt 2 64) (+ ahi bhi)))))
    (values (ldb (byte 64 0) sum)
            (ldb (byte 64 64) sum))))

(defun sub128 (alo ahi blo bhi)
  "Compute

    (alo + ahi*2^64) - (blo + bhi*2^64)

for A >= B.
"
  #+hypergeometrica-intrinsics
  (sub128 alo ahi blo bhi)
  #-hypergeometrica-intrinsics
  (let ((sum (+ (- alo blo) (* (expt 2 64) (- ahi bhi)))))
    (assert (not (minusp sum)))
    (values (ldb (byte 64 0) sum)
            (ldb (byte 64 64) sum))))



(declaim (inline complement-digit))
(defun complement-digit (n)
  (declare (type digit n))
  (logxor n $digit-ones))
