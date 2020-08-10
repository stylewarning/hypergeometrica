;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:hypergeometrica)

;;; Non-math utilities

(defmacro do-range ((var from to &optional result) &body body)
  (alexandria:once-only (from to)
    `(do ((,var ,from (1+ ,var)))
         ((= ,var ,to) ,result)
       ,@body)))

(defmacro define-symbolic-enumeration (name &body symbols)
  (multiple-value-bind (symbols decls docs)
      (alexandria:parse-body symbols :documentation t)
    (declare (ignore decls))
    `(progn
       (deftype ,name ()
         ,@(if docs (list docs) nil)
         '(member ,@symbols))
       ,@(loop :for sym :in symbols
               :collect `(defconstant ,sym ',sym)))))
