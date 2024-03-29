;;;; config.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defvar *verbose* nil)
(defvar *hypergeometrica-log-stream* *standard-output*)

;;; Enable parallelism in some routines. This could make it harder to
;;; debug or profile.

;;; (push :hypergeometrica-parallel *features*)

#+ (and hypergeometrica-parallel lparallel)
(unless (and (boundp 'lparallel:*kernel*)
             (not (null lparallel:*kernel*)))
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "Hypergeometrica")))


;;; Enable assembly intrinsics.

;; N.B.: Intrinsic functions are still compiled, even if they're not
;; used by Hypergeometrica.
#+(and #:disable sbcl (or x86-64))
(push :hypergeometrica-intrinsics *features*)


;;; Enable cheap-ish safety checks. This may slow down code, but help debug.

(push :hypergeometrica-safe *features*)


;;; Are you paranoid about the correctness of things? This is the
;;; feature for you.

(push :hypergeometrica-paranoid *features*)


;;; Enable explicit and ruthless initialization of objects. Don't
;;; trust "re-used" objects.

(push :hypergeometrica-hygiene *features*)


;;; Enable the use of floating point FFTs for smallish inputs.

;; (push :hypergeometrica-floating-point *features*)


;;; Optimization qualities

;; It is useful to change these when debugging.

(defparameter *optimize-extremely-safely* '(optimize (speed 0) safety debug (space 0) (compilation-speed 0)))

(defparameter *optimize-dangerously-fast* '(optimize speed (safety 0) (space 0) (space 0) (compilation-speed 0)))


;;; Storage constants

(defvar *maximum-file-size* (* 16 (expt 1024 3)) ; XXX: needed?
  "The maximum size of a file in octets.")

(defvar *maximum-vector-size* (* 8 (expt 1024 2))
  "The maximum size of a vector that can be stored in memory in octets.")

(defvar *default-file-directory* (uiop:ensure-directory-pathname "/tmp/"))

(defun hypergeometrica-work-directory ()
  "Return the pathname of a directory where work files may be written to. This is controlled in order of priority by:

    Environment variable: HYPERGEOMETRICA_WORK

    Lisp variable: HYPERGEOMETRICA::*DEFAULT-FILE-DIRECTORY*
"
  (let ((env (uiop:getenv "HYPERGEOMETRICA_WORK")))
    (if (null env)
        *default-file-directory*
        (uiop:ensure-directory-pathname env))))
