;;;; package.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(fiasco:define-test-package #:hypergeometrica-tests
  (:use #:hypergeometrica)
  (:local-nicknames (:h :hypergeometrica))
  (:export
   #:test-hypergeometrica))
