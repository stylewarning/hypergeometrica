;;;; mpf.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:hypergeometrica)

(deftype rounding-mode ()
  `(member rndn
           rndz
           rndd
           rndu
           rndna
           rnda
           rndf))

(define-symbolic-constant rndn "Round to nearest, ties with even.")
(define-symbolic-constant rndz "Round toward zero.")
(define-symbolic-constant rndd "Round down, toward -inf.")
(define-symbolic-constant rndu "Round up, toward +inf.")
(define-symbolic-constant rndna "Round to nearest, ties away from zero.")
(define-symbolic-constant rnda "Round away from zero.")
(define-symbolic-constant rndf "Faithful rounding")

(defconstant $exp-zero (minimum-signed-byte 32))
(defconstant $exp-inf (1- (maximum-signed-byte 32)))
(defconstant $exp-nan (maximum-signed-byte 32))

(defstruct (mpf (:predicate mpf?)
                (:copier nil))
  (sign 1                   :type sign)
  (expn $exp-zero           :type (signed-byte 32))
  (storage (make-storage 0) :type storage :read-only t))


;;; Comparison functions

(defun mpf-finite? (mpf))
(defun mpf-nan? (mpf))
(defun mpf-zerop (mpf))


(defun mpf-negate (mpf))
(defun mpf-negate! (mpf))

