;;;; disk-vec.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun generate-work-filename ()
  (merge-pathnames (format nil "hypergeo-r~X-t~D"
                           (random most-positive-fixnum)
                           (get-internal-real-time))
                   (hypergeometrica-work-directory)))

(defun write-zeros-to-file (filename n)
  (assert (uiop:file-pathname-p filename))
  (check-type n alexandria:array-length)
  (with-open-file (s filename :direction ':output
                              :if-does-not-exist ':create
                              :if-exists ':supersede
                              :element-type 'digit)
    (loop :repeat n :do (write-byte 0 s)))
  nil)

(defclass disk-vec ()
  ((mmap-data :initarg :mmap-data
              :reader disk-vec.mmap-data)
   (filename :initarg :filename
             :reader disk-vec.filename)
   (length   :initarg :length
             :reader vec-digit-length)))

(defmethod vec-digit-pointer ((vec disk-vec))
  (mmap-data-pointer (disk-vec.mmap-data vec)))

(defun disk-vec-finalizer (disk-vec)
  (let ((filename (disk-vec.filename disk-vec))
        (mmap-data (disk-vec.mmap-data disk-vec)))
    (lambda ()
      (when (uiop:file-exists-p filename)
        (munmap mmap-data)
        (delete-file filename)))))

(defmethod free-vec ((vec disk-vec))
  (funcall (disk-vec-finalizer vec))
  nil)

(defun make-disk-vec (n)
  (let ((filename (generate-work-filename)))
    (write-zeros-to-file filename n)
    (let ((vec (make-instance 'disk-vec :mmap-data (mmap filename (ceiling (* n $digit-bits) 8))
                                        :filename filename
                                        :length n)))
      (when *auto-free-vecs*
        (tg:finalize vec (disk-vec-finalizer vec)))
      vec))
  )
