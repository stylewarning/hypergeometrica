;;;; storage.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; Storage Protocol

(defgeneric storage-length (s)
  (:documentation "How many elements are storable in S?"))

(defgeneric resize-storage (s n)
  (:documentation "Adjust the length of the storage S to a length of N elements.")
  (:method :before (s n)
    (assert (plusp n) () "Attempting to resize storage to ~D elements" n)))

(defgeneric resize-storage-by (s delta)
  (:documentation "Adjust the length of the storage S by N elements.")
  (:method (s delta)
    (resize-storage s (+ delta (storage-length s)))))

(defgeneric optimize-storage (s)
  (:method (s)
    s))

(defgeneric storage-ref (s n)
  (:documentation "Get the Nth element of the storage S. This may not be the most efficient method in doing so."))

(defgeneric (setf storage-ref) (new-value s n)
  (:documentation "Set the Nth element of the storage S to NEW-VALUE. This may not be the most efficient method in doing so."))

(defgeneric copy-storage (s)
  (:documentation "Produce a copy of the storage S."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; RAM Storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype storage ()
  `(and (array digit (*))
        (not simple-array)))

(deftype raw-storage ()
  #+sbcl
  `(simple-array digit (*))
  #-sbcl
  `storage)

(defun make-storage (n)
  (assert (plusp n))
  (make-array n :element-type 'digit
                :initial-element 0
                :adjustable t))

(declaim (inline raw-storage-of-storage)
         (ftype (function (storage) raw-storage) raw-storage-of-storage))
(defun raw-storage-of-storage (a)
  (declare (type storage a))
  #+sbcl
  (sb-ext:array-storage-vector a)
  #-sbcl
  a)

(defmethod resize-storage ((a vector) n)
  (declare (type storage a)
           (type alexandria:array-length n))
  (unless (= n (length a))
    (adjust-array a n :initial-element 0))
  (values))

(defmethod storage-ref ((s vector) n)
  (aref s n))

(defmethod (setf storage-ref) (new-value (s vector) n)
  (setf (aref s n) new-value))

(defmethod storage-length ((s vector))
  (length s))

(defmethod copy-storage ((s vector))
  (copy-seq s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Disk Storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun seek (stream position)
  (unless (file-position stream position)
    (error "Seek ~A to ~D failed." stream position)))

(defclass file-backed-storage ()
  ((length :initarg :length
           :reader %storage-length)
   (pathname :initarg :pathname
             :reader file-backed-storage-pathname)
   (stream :initarg :stream
           :reader file-backed-storage-stream
           :type file-stream))
  (:documentation "Storage backed by a single file."))

(defmethod storage-length ((s file-backed-storage))
  (let ((recorded-length (%storage-length s))
        (actual-length (file-length (file-backed-storage-stream s))))
    (when (> actual-length (1+ recorded-length))
      (warn "The storage ~A has a recorded length of ~D but is actually ~D."
            s
            recorded-length
            actual-length))
    recorded-length))

(defun rewind-file-backed-storage (fbs)
  (seek (file-backed-storage-stream fbs) 0))

(defmethod resize-storage ((s file-backed-storage) n)
  (let ((length (storage-length s)))
    #+sbcl
    (unless (zerop (sb-posix:ftruncate (sb-sys:fd-stream-fd
                                        (file-backed-storage-stream s))
                                       (* 8 n)))
      (error "Error FTRUNCATE on ~A" s))
    (when (> n length)
      (let ((stream (file-backed-storage-stream s)))
        (seek stream length)
        (loop :repeat (- n length)
              :do (write-byte 0 stream))))
    (setf (slot-value s 'length) n)
    (values)))

(defmethod storage-ref ((s file-backed-storage) n)
  #+hypergeometrica-paranoid
  (check-type n alexandria:array-index)
  #+hypergeometrica-paranoid
  (assert (<= 0 n (1- (storage-length s))))
  (let ((stream (file-backed-storage-stream s)))
    (seek stream n)
    (read-byte stream)))

(defmethod (setf storage-ref) (new-value (s file-backed-storage) n)
  #+hypergeometrica-paranoid
  (check-type n alexandria:array-index)
  #+hypergeometrica-paranoid
  (check-type new-value digit)
  #+hypergeometrica-safe
  (assert (<= 0 n (1- (storage-length s))))
  (let ((stream (file-backed-storage-stream s)))
    (seek stream n)
    (write-byte new-value stream)))

(defun generate-fbs-pathname ()
  (merge-pathnames (format nil "hypergeo-r~X-t~D"
                           (random most-positive-fixnum)
                           (get-internal-real-time))
                   *default-file-directory*))

(defun make-file-backed-storage (n)
  (assert (<= 0 n (/ *maximum-file-size* 8)))
  (let* ((filename (generate-fbs-pathname))
         (stream (open filename :direction ':io
                                :if-exists ':error
                                :if-does-not-exist ':create
                                :element-type '(unsigned-byte 64))))
    (when *verbose*
      (format t "~&Created file of ~D byte~:P: ~A~%" (* 8 n) filename))
    (loop :repeat n :do (write-byte 0 stream))
    (let ((st (make-instance 'file-backed-storage
                             :length n
                             :pathname filename
                             :stream stream)))
      (tg:finalize st (lambda ()
                        (close stream :abort t)
                        (when (uiop:file-exists-p filename)
                          (delete-file filename))))
      (rewind-file-backed-storage st)
      st)))

(defmethod copy-storage ((s file-backed-storage))
  (rewind-file-backed-storage s)
  (let ((copy (make-file-backed-storage (storage-length s))))
    (dotimes (i (storage-length s))
      (setf (storage-ref copy i) (storage-ref s i)))
    (rewind-file-backed-storage copy)
    (rewind-file-backed-storage s)
    copy))

(defmethod optimize-storage ((s file-backed-storage))
  (let ((len (storage-length s)))
    (rewind-file-backed-storage s)
    (loop :for i :from (1- len) :downto 0
          :while (zerop (storage-ref s i))
          :finally (resize-storage s (1+ i)))
    (rewind-file-backed-storage s)
    s))

(defun file-backed-storage-to-storage (fbs)
  (rewind-file-backed-storage fbs)
  (let ((s (make-storage (storage-length fbs))))
    (read-sequence s (file-backed-storage-stream fbs))
    s))

(defun storage-to-file-backed-storage (s)
  (let ((fs (make-file-backed-storage (storage-length s))))
    (write-sequence s (file-backed-storage-stream fs))
    (rewind-file-backed-storage fs)
    fs))
