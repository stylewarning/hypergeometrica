;;;; config.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defvar *verbose* nil)

(setf lparallel:*kernel* (lparallel:make-kernel 8 :name "Hypergeometrica"))
