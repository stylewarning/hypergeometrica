;;;; logging.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

(defun dbg (control &rest args)
  (when (boundp '*hypergeometrica-log-stream*)
    (fresh-line *hypergeometrica-log-stream*)
    (format *hypergeometrica-log-stream* control args)
    (terpri *hypergeometrica-log-stream*)
    nil))
