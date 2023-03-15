;;; pi.lisp
;;;
;;; Copyright (c) 2023 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-pi ()
  (h::test-pi 5 :from 1
                :check (lambda (n)
                         (is (zerop n)))))
