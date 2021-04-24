;;;; ram-vec.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

;;; Some C stuff first...

(cffi:defcfun malloc :pointer
  (n-bytes :size))

(cffi:defcfun realloc :pointer
  (src     :pointer)
  (n-bytes :size))

(cffi:defcfun calloc :pointer
  (num-elts :size)
  (elt-size :size))

(cffi:defcfun free :void
  (ptr :pointer))

(cffi:defcfun memcpy :pointer
  (dest      :pointer)
  (src       :pointer)
  (num-bytes :size))


;;; Now the RAM-VEC implementation.

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
                   :documentation "The number of digits this VEC holds.")
   (finalizer-cons :initarg :finalizer-cons
                   :reader ram-vec.finalizer-cons
                   :documentation "A CONS whose CAR will be read in order to free the mrmory of the RAM-VEC. This needed indirection is used in case the BASE-POINTER changes."))
  (:documentation "Digits allocated in RAM."))

(defmethod print-object ((vec ram-vec) stream)
  (print-unreadable-object (vec stream :type t :identity t)
    (format stream "~D digit~:P" (vec-digit-length vec))))

(defun alloc (num-bytes)
  "Allocated NUM-BYTES bytes, initialized to zero."
  #+hypergeometrica-safe
  (check-type num-bytes alexandria:array-length)
  (let ((pointer (calloc num-bytes 1)))
    (when (cffi:null-pointer-p pointer)
      (error "CALLOC failed."))
    pointer))

(defun uninitialized-alloc (num-bytes)
  "Allocated NUM-BYTES bytes."
  #+hypergeometrica-safe
  (check-type num-bytes alexandria:array-length)
  (let ((pointer (malloc num-bytes)))
    (when (cffi:null-pointer-p pointer)
      (error "MALLOC failed"))
    pointer))

(defun resize-alloc (pointer num-bytes)
  "Re-allocate POINTER with NUM-BYTES bytes. Return two values:"
  #+hypergeometrica-safe
  (check-type num-bytes alexandria:array-length)
  (let ((new-pointer (realloc pointer num-bytes)))
    (when (cffi:null-pointer-p new-pointer)
       (error "REALLOC failed"))
    new-pointer))

(defun make-ram-vec (n)
  (check-type n alexandria:array-length)
  (let* ((num-bytes (bytes-for-digits n))
         (pointer (alloc num-bytes))
         (finalizer-cons (cons pointer nil))
         (vec (make-instance 'ram-vec :allocated-size num-bytes
                                      :base-pointer pointer
                                      :length n
                                      :finalizer-cons finalizer-cons)))
    (when *auto-free-vecs*
      (tg:finalize vec (lambda () (unless (cffi:null-pointer-p (car finalizer-cons))
                                    (free (car finalizer-cons))))))
    vec))

(defmethod copy-vec ((vec ram-vec))
  (let* ((bytes (ram-vec.allocated-size vec))
         (pointer (uninitialized-alloc bytes))
         (finalizer-cons (cons pointer nil))
         (copy (make-instance 'ram-vec :allocated-size bytes
                                       :base-pointer pointer
                                       :length (vec-digit-length vec)
                                       :finalizer-cons finalizer-cons
                                       )))
    (memcpy pointer (ram-vec.base-pointer vec) bytes)
    (when *auto-free-vecs*
      (tg:finalize copy (lambda () (unless (cffi:null-pointer-p (car finalizer-cons))
                                    (free (car finalizer-cons))))))
    copy))

(defmethod resize-vec-by ((vec ram-vec) n-digits)
  (let* ((new-length (+ n-digits (vec-digit-length vec)))
         (new-allocated-size (bytes-for-digits new-length)))
    #+hypergeometrica-safe
    (check-type new-length unsigned-byte)
    (let* ((old-pointer (ram-vec.base-pointer vec))
           (new-pointer (realloc old-pointer new-allocated-size)))
      (setf (slot-value vec 'length)         new-length
            (slot-value vec 'allocated-size) new-allocated-size)
      (unless (cffi:pointer-eq old-pointer new-pointer)
        (setf (slot-value vec 'base-pointer)   new-pointer
              (slot-value vec 'finalizer-cons) (cons new-pointer nil)))
      nil)))

(defmethod free-vec ((vec ram-vec))
  (when (plusp (ram-vec.allocated-size vec))
    (free (ram-vec.base-pointer vec))
    (setf (slot-value vec 'base-pointer) (cffi:null-pointer)
          (slot-value vec 'allocated-size) 0
          (slot-value vec 'length) 0
          (slot-value vec 'finalizer-cons) (cons (cffi:null-pointer) nil)))
  nil)

