;;;; config.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defvar *verbose* nil)

(setf lparallel:*kernel* (lparallel:make-kernel 8 :name "Hypergeometrica"))

;; (push :hypergeometrica-parallel *features*)

;;; Enable safety checks. This may slow down code, but help debug.
(push :hypergeometrica-safe *features*)

;;; Enable explicit and ruthless initialization of objects. Don't
;;; trust "re-used" objects.
(push :hypergeometrica-hygiene *features*)
