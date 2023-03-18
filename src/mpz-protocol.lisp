;;;; mpz-protocol.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

;;; PRINT-OBJECT is a required protocol method.

(defgeneric mpz-integer (mpz)
  (:documentation "Convert an MPZ to a Common Lisp integer."))

(defgeneric integer-mpz (n class-name)
  (:documentation "Convert the Common Lisp integer N to an MPZ-like object of class CLASS-NAME."))

(defgeneric mpz-size (mpz)
  (:documentation "The number of digits required to mathematically represent MPZ. If the MPZ is 0, then this is 0."))

(defgeneric mpz-bit-size (mpz)
  (:documentation "The number of bits required to mathematically represent MPZ in two's complement."))

(defun mpz-integer-length (mpz)
  (mpz-bit-size mpz))

(defgeneric optimize-mpz (mpz)
  (:documentation "Optimize the storage used to represent the MPZ."))

(defgeneric mpz-digit (mpz n)
  (:documentation "Get the Nth least-significant digit from MPZ."))

(defgeneric mpz-set-zero! (mpz)
  (:documentation "Mutate the MPZ so that it is equal to zero."))

(defgeneric mpz-zerop (mpz)
  (:documentation "Is MPZ zero?"))

(defgeneric mpz-plusp (mpz)
  (:documentation "Is MPZ positive?"))

(defgeneric mpz-minusp (mpz)
  (:documentation "Is MPZ negative?"))

(defgeneric mpz-abs (mpz)
  (:documentation "Return the absolute value of MPZ. Shares storage."))

(defgeneric mpz-negate (mpz)
  (:documentation "Return the negative of MPZ."))

(defgeneric mpz-negate! (mpz)
  (:documentation "Mutate MPZ so that it is negative."))

(defgeneric mpz-= (a b)
  (:documentation "Are the MPZs A and B mathematically equal?"))

(defun mpz-/= (a b)
  (not (mpz-= a b)))

(defgeneric mpz-> (a b)
  (:documentation "Is the MPZ A greater than the MPZ B?"))

(defun mpz->= (a b)
  (or (mpz-= a b)
      (mpz-> a b)))

(defun mpz-< (a b)
  (not (mpz->= a b)))

(defun mpz-<= (a b)
  (not (mpz-> a b)))

(defgeneric mpz-+ (a b))
(defgeneric mpz-1+ (a))
(defgeneric mpz-- (a b))
(defgeneric mpz-1- (a))
(defgeneric mpz-* (a b))
(defgeneric mpz-left-shift (a n))
(defgeneric mpz-right-shift (a n))
(defgeneric mpz-multiply-by-digit! (d mpz))
(defgeneric mpz-multiply-by-s64! (d mpz))
(defgeneric mpz-debug (mpz &optional stream))
