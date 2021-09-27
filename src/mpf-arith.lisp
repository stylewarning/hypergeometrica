;;;; mpf-arith.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun mpf-negate! (x)
  (setf (mpf-sign x) (* -1 (mpf-sign x)))
  nil)


