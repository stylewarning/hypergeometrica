;;;; config.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defvar *verbose* nil)

(setf lparallel:*kernel* (lparallel:make-kernel 8 :name "Hypergeometrica"))

;;; Enable parallelism in some routines. This could make it harder to
;;; debug or profile.
#+#:disabled(push :hypergeometrica-parallel *features*)

;;; Enable cheap-ish safety checks. This may slow down code, but help debug.
(push :hypergeometrica-safe *features*)

;;; Are you paranoid about the correctness of things? This is the
;;; feature for you.
;(push :hypergeometrica-paranoid *features*)

;;; Enable explicit and ruthless initialization of objects. Don't
;;; trust "re-used" objects.
(push :hypergeometrica-hygiene *features*)

;;(push :hypergeometrica-floating-point *features*)

