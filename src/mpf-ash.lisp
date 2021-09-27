;;;; mpf-ash.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun mpf-ash! (x n)
  (cond
    ((mpf-nan? x)      x)
    ((mpf-infinite? x) x)
    ((mpf-zero? x)     x)
    (t
     (incf (mpf-exponent x) n))))
