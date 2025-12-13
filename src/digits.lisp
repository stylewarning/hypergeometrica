;;;; digits.lisp
;;;;
;;;; Copyright (c) 2025 Robert Smith

(defpackage #:hypergeometrica/digits
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:h #:hypergeometrica)
   (#:digit #:hypergeometrica/digit))
  (:shadow
   #:length)
  (:export
   #:digits                             ; TYPE
   #:alloc                              ; FUNCTION
   #:free                               ; FUNCTION
   #:length                             ; FUNCTION
   #:ref                                ; ACCESSOR
   #:copy                               ; FUNCTION
   #:digits->vector                     ; FUNCTION
   ))

(in-package #:hypergeometrica/digits)

;;; This file contains in-memory sequences of digits. These are used
;;; as a low-level data structure for building number abstractions.
;;;
;;; This file was originally written with CFFI, however, it wasn't
;;; working when I was using MEM-REF with negative indexes.

(coalton-toplevel
  (repr :native sb-sys:system-area-pointer)
  (define-type Digits)
  )

(deftype digits ()
  'sb-sys:system-area-pointer)

(defun alloc (num-digits)
  "Allocated NUM-DIGITS digits."
  (let ((pointer (sb-sys:allocate-system-memory (* digit:$digit-bytes (1+ num-digits)))))
    (setf (sb-sys:sap-ref-64 pointer 0) num-digits)
    (sb-sys:sap+ pointer digit:$digit-bytes)))

(defun free (pointer)
  (let ((pointer (sb-sys:sap+ pointer (- digit:$digit-bytes))))
    (sb-sys:deallocate-system-memory
     pointer
     (+ digit:$digit-bytes (sb-sys:sap-ref-64 pointer 0))))
  nil)

(defun length (pointer)
  (sb-sys:sap-ref-64 pointer (- digit:$digit-bytes)))

(defun ref (digits index)
  #+hypergeometrica-paranoid
  (assert (and (not (cffi:null-pointer-p digits))
               (<= 0 index (1- (length digits)))))
  (sb-sys:sap-ref-64 digits index))

(defun (setf ref) (new-digit digits index)
  #+hypergeometrica-paranoid
  (assert (and (not (cffi:null-pointer-p digits))
               (<= 0 index (1- (length digits)))))
  #+hypergeometrica-paranoid
  (check-type new-digit digit:digit)
  (setf (sb-sys:sap-ref-64 digits index) new-digit))

(defun copy (digits)
  (let* ((len (length digits))
         (new-ptr (alloc len)))
    ;; TODO: memcpy or equivalent
    (dotimes (i len new-ptr)
      (setf (ref new-ptr i) (ref digits i)))))

(defun digits->vector (digits)
  (loop :with len := (length digits)
        :with vector := (make-array len :element-type 'digit:digit :initial-element 0)
        :for i :below len
        :do (setf (aref vector i) (ref digits i))
        :finally (return vector)))

