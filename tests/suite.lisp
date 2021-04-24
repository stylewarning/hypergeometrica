;;;; suite.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(defun test-hypergeometrica ()
  (let ((h::*verbose* nil))
    (run-package-tests :package ':hypergeometrica-tests
                       :verbose nil
                       :describe-failures t
                       :interactive t)))
