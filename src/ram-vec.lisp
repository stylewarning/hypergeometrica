;;;; ram-vec.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defclass ram-vec ()
  ((allocated-size :initarg :allocated-size
                   :reader ram-vec.allocated-size
                   :documentation "The allocation size in bytes.")
   (base-pointer   :initarg :base-pointer
                   :reader ram-vec.base-pointer
                   :reader vec-digit-pointer
                   :documentation "The base pointer that was allocated. This is a pointer that would be able to be freed.")
   ;; TODO: This can be derived from ALLOCATED-SIZE
   (length         :initarg :length
                   :reader vec-digit-length
                   :documentation "The number of digits this VEC holds."))
  (:documentation "Digits allocated in RAM."))

(defmethod print-object ((vec ram-vec) stream)
  (print-unreadable-object (vec stream :type t :identity t)
    (format stream "~D digit~:P" (vec-digit-length vec))))

(defun make-ram-vec (n)
  (check-type n alexandria:array-length)
  (let* ((num-bytes (ceiling (* n $digit-bits) 8))
         (pointer (cffi:foreign-alloc ':uint8 :initial-element 0
                                      :count num-bytes))
         (vec (make-instance 'ram-vec :allocated-size num-bytes
                                      :base-pointer pointer
                                      :length n)))
    (when *auto-free-vecs*
      (tg:finalize vec (lambda () (cffi:foreign-free pointer))))
    vec))

(cffi:defcfun memcpy :pointer
  (dest :pointer)
  (src  :pointer)
  (n    :size))

(defmethod copy-vec ((vec ram-vec))
  (let* ((bytes (ram-vec.allocated-size vec))
         (pointer (cffi:foreign-alloc ':uint8 :count bytes))
         (copy (make-instance 'ram-vec :allocated-bytes bytes
                                       :base-pointer pointer
                                       :length (vec-digit-length vec))))
    (memcpy pointer (ram-vec.base-pointer vec) bytes)
    (when *auto-free-vecs*
      (tg:finalize copy (lambda () (cffi:foreign-free pointer))))
    copy))

(defmethod free-vec ((vec ram-vec))
  (when (plusp (ram-vec.allocated-size vec))
    (cffi:foreign-free (ram-vec.base-pointer vec))
    (setf (slot-value vec 'allocated-size) 0
          (slot-value vec 'length) 0))
  nil)

