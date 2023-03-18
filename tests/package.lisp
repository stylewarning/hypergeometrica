;;;; package.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(fiasco:define-test-package #:hypergeometrica-tests
  (:local-nicknames (#:h #:hypergeometrica))
  (:export
   #:test-hypergeometrica))

(cl:defpackage #:hypergeometrica-debug
  (:use #:cl)
  (:local-nicknames (#:h #:hypergeometrica))
  (:export
   #:dd))
