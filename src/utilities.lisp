;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:hypergeometric)

;;; Non-math utilities

(defmacro do-range ((var from to &optional result) &body body)
  (alexandria:once-only (from to)
    `(do ((,var ,from (1+ ,var)))
         ((= ,var ,to) ,result)
       ,@body)))

(defmacro define-symbolic-constant (name documentation)
  (check-type name (and symbol (not keyword)))
  (check-type documentation string)
  `(defconstant ,name ',name ,documentation))
