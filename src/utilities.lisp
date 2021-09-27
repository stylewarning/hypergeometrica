;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defmacro do-range ((var from to &optional result) &body body)
  (alexandria:once-only (from to)
    `(do ((,var ,from (1+ ,var)))
         ((= ,var ,to) ,result)
       ,@body)))

(defmacro define-symbolic-enumeration (name &body symbols)
  `(progn
     (deftype ,name ()
       '(member ,@symbols))
     ,@(loop :for s :in symbols
             :collect `(defconstant ,s ',s))))

(defun bit-mask (start end)
  "Produce an integer containing the number 1 from and including bit at position START to bit at position END."
  (dpb -1 (byte (1+ (- end start)) start) 0))

(defun boolean->bit (thing)
  #+hypergeometrica-paranoid
  (when (typep thing 'bit)
    (warn "Converting a BIT (as a BOOLEAN) to a BIT."))
  (if thing 1 0))

(defun bit->boolean (bit)
  (ecase bit
    (0 nil)
    (1 t)))
