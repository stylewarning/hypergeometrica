;;;; storage.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype storage ()
  `(or ram-vec disk-vec))

(defun make-storage (n)
  (check-type n alexandria:array-length)
  (cond
    ((<= 0 n (floor (* 8 *maximum-vector-size*) $digit-bits))
     (make-ram-vec n))
    (t
     (when *verbose*
       (format t "~&Making DISK-VEC of ~D digit~:P~%" n))
     (make-disk-vec n))))
