;;;; comparison.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(define-symbolic-enumeration comparison
  cmp/lt                                ; Less
  cmp/eq                                ; Equal
  cmp/gt                                ; Greater
  cmp/??)                               ; Unordered

(defun flip-comparison (c)
  (ecase c
    (cmp/lt cmp/gt)
    (cmp/eq cmp/eq)
    (cmp/gt cmp/lt)
    (cmp/?? cmp/??)))

