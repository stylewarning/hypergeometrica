;;;; math-utilities.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defmacro do-range ((var from to &optional result) &body body)
  (alexandria:once-only (from to)
    `(do ((,var ,from (1+ ,var)))
         ((= ,var ,to) ,result)
       ,@body)))

(defun power-of-two-p (n)
  "Is N a power-of-two?"
  (and (plusp n)
       (zerop (logand n (1- n)))))

(defun next-power-of-two (n)
  "Find the minimum K such that N <= 2^K."
  (if (power-of-two-p n)
      (1- (integer-length n))
      (integer-length n)))

(defun least-power-of-two->= (n)
  "What is the least power-of-two greater than or equal to N?"
  (if (power-of-two-p n)
      n
      (ash 1 (integer-length n))))
