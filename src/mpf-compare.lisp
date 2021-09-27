;;;; mpf-compare.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun mpf-compare (x y)
  (error "stub"))

(defun mpf< (x y)
  (ecase (mpf-compare x y)
    (cmp/lt t)
    (cmp/eq nil)
    (cmp/gt nil)))

(defun mpf<= (x y)
  (ecase (mpf-compare x y)
    (cmp/lt t)
    (cmp/eq t)
    (cmp/gt nil)))

(defun mpf= (x y)
  (ecase (mpf-compare x y)
    (cmp/lt nil)
    (cmp/eq t)
    (cmp/gt nil)))

(defun mpf> (x y)
  (ecase (mpf-compare x y)
    (cmp/lt nil)
    (cmp/eq nil)
    (cmp/gt t)))

(defun mpf>= (x y)
  (ecase (mpf-compare x y)
    (cmp/lt nil)
    (cmp/eq t)
    (cmp/gt t)))

