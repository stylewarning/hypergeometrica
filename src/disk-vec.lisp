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
              :accessor disk-vec.mmap-data)
   (filename :initarg :filename
             :reader disk-vec.filename)
   (length   :initarg :length
             :reader vec-digit-length
             :writer disk-vec.set-length)))

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

(defmethod copy-vec ((vec disk-vec))
  (let* ((n (vec-digit-length vec))
         (new-vec (make-disk-vec n)))
    (with-vecs (vec vec_ new-vec new-vec_)
      (dotimes (i n new-vec)
        (setf (vec_ i) (new-vec_ i))))))

;;; VEC-REF implemented by default

(defmethod resize-vec-by ((vec disk-vec) n)
  (let* ((old-length (vec-digit-length vec))
         (new-length (+ n old-length))
         (new-size-bytes (bytes-for-digits new-length)))
    (cond
      ((minusp new-length)
       (error "can't resize to a negative size..."))
      ((zerop n)
       vec)
      (t
       (tg:cancel-finalization vec)
       (munmap (disk-vec.mmap-data vec))
       ;; TRUNCATE will write 0 to extra bytes.
       (sb-posix:truncate (disk-vec.filename vec) new-size-bytes)
       (setf (disk-vec.mmap-data vec) (mmap (disk-vec.filename vec) new-size-bytes))
       (disk-vec.set-length new-length vec)
       (when *auto-free-vecs*
        (tg:finalize vec (disk-vec-finalizer vec)))
       vec))))

(defun make-disk-vec (n)
  (let ((filename (generate-work-filename)))
    (write-zeros-to-file filename n)
    (make-disk-vec-from-file n filename)))

(defun make-disk-vec-from-file (n filename)
  (let ((vec (make-instance 'disk-vec
               :mmap-data (mmap filename (bytes-for-digits n))
               :filename filename
               :length n)))
      (when *auto-free-vecs*
        (tg:finalize vec (disk-vec-finalizer vec)))
      vec))
