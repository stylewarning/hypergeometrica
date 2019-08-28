;;;; digit.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defconstant $base (1+ most-positive-fixnum))
(defconstant $digit-bits (1- (integer-length $base)))

(deftype digit ()
  "A digit in an MPZ."
  `(integer 0 (,$base)))

(deftype simple-digit-vector ()
  '(simple-array digit (*)))

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
