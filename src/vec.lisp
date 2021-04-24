;;;; vec.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)


;;; VEC Protocol

(declaim (ftype (function (t) cffi:foreign-pointer) vec-digit-pointer))
(defgeneric vec-digit-pointer (vec)
  (:documentation "Retrieve a pointer to readable/writable digits for VEC."))

(declaim (ftype (function (t) alexandria:array-length) vec-digit-length))
(defgeneric vec-digit-length (vec)
  (:documentation "Retrieve the number of digits held by VEC."))

(defgeneric copy-vec (vec)
  (:documentation "Produce a copy of VEC."))

(defgeneric resize-vec-by (vec n-digits)
  (:documentation "Change the capacity of VEC by N-DIGITS digits."))

(defgeneric free-vec (vec)
  (:documentation "Free all memory associated with VEC."))

(defgeneric vec-ref (vec i)
  (:documentation "Reference the Ith element of a vector VEC. It is advised to use WITH-VEC when possible instead of this."))

(defparameter *auto-free-vecs* t
  "Automatically free VECs during garbage collection?")


;;; Convenient VEC Access & Other Pleasantries

(declaim (inline read-digit-pointer write-digit-pointer))
(defun read-digit-pointer (digit-pointer index)
  (declare (type alexandria:array-index index)
           (type cffi:foreign-pointer digit-pointer)
           #.*optimize-dangerously-fast*
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (ecase $digit-bits
    (8  (cffi:mem-aref digit-pointer ':uint8 index))
    (16 (cffi:mem-aref digit-pointer ':uint16 index))
    (32 (cffi:mem-aref digit-pointer ':uint32 index))
    (64 (cffi:mem-aref digit-pointer ':uint64 index))))

(defun write-digit-pointer (digit-pointer index value)
  (declare (type alexandria:array-index index)
           (type cffi:foreign-pointer digit-pointer)
           (type digit value)
           #.*optimize-dangerously-fast*
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (ecase $digit-bits
    (8  (setf (cffi:mem-aref digit-pointer ':uint8 index) value))
    (16 (setf (cffi:mem-aref digit-pointer ':uint16 index) value))
    (32 (setf (cffi:mem-aref digit-pointer ':uint32 index) value))
    (64 (setf (cffi:mem-aref digit-pointer ':uint64 index) value))))

(defmacro with-vec ((vec accessor) &body body)
  (alexandria:with-gensyms (pointer i new-digit)
    (alexandria:once-only (vec)
      `(let ((,pointer (vec-digit-pointer ,vec)))
         (labels ((,accessor (,i)
                    #+hypergeometrica-paranoid
                    (assert (<= 0 ,i (vec-digit-length ,vec)))
                    (read-digit-pointer ,pointer ,i))
                  ((setf ,accessor) (,new-digit ,i)
                    #+hypergeometrica-paranoid
                    (assert (<= 0 ,i (vec-digit-length ,vec)))
                    (write-digit-pointer ,pointer ,i ,new-digit)))
           (declare (inline ,accessor (setf ,accessor))
                    (ignorable #',accessor #'(setf ,accessor)))
           ,@body)))))

(defmethod vec-ref (vec i)
  (with-vec (vec vec_)
    (vec_ i)))

(defmacro with-vecs (vec-accessors &body body)
  (cond
    ((null vec-accessors)
     `(progn ,@body))
    ((null (rest vec-accessors))
     (error "bad WITH-VECS syntax"))
    (t
     `(with-vec (,(first vec-accessors) ,(second vec-accessors))
        (with-vecs ,(cddr vec-accessors)
          ,@body)))))

(defun vec->vector (vec &optional (start 0) (end (vec-digit-length vec)))
  (assert (<= 0 start end (vec-digit-length vec)))
  (let ((vector (make-array (- end start) :element-type 'digit
                                          :initial-element 0)))
    (with-vec (vec vec_)
      (loop :for i :of-type alexandria:array-index
            :from start :below end :do
              (setf (aref vector i) (vec_ i)))
      vector)))

(defmacro do-digits ((i digit vec &optional result) &body body)
  (alexandria:with-gensyms (accessor)
    (alexandria:once-only (vec)
      `(with-vec (,vec ,accessor)
         (dotimes (,i (vec-digit-length ,vec) ,result)
           (let* ((,digit (,accessor ,i))
                  (,i ,i))
             ,@body))))))

(defun vec= (a b)
  (let ((a-length (vec-digit-length a))
        (b-length (vec-digit-length b)))
    (and (= a-length b-length)
         (with-vecs (a a_ b b_)
           (loop :for i :below a-length
                 :always (= (a_ i) (b_ i)))))))

(defun vec-fill (vec digit)
  (with-vec (vec vec_)
    (dotimes (i (vec-digit-length vec))
      (setf (vec_ i) digit))))

(defun vec-every (fun vec)
  (with-vec (vec vec_)
    (loop :for i :below (vec-digit-length vec)
          :always (funcall fun (vec_ i)))))

(defun vec-into (vec fun)
  (with-vec (vec vec_)
    (dotimes (i (vec-digit-length vec) vec)
      (setf (vec_ i) (funcall fun)))))

(defun vec-replace/unsafe (dst src &key (start1 0))
  (let ((written-length
          (min (- (vec-digit-length dst) start1)
               (vec-digit-length src))))
    (cond
      ((minusp written-length)
       (warn "Attempting VEC-REPLACE/UNSAFE with OOB offset"))
      (t
       (memcpy (cffi:inc-pointer (vec-digit-pointer dst) (bytes-for-digits start1))
               (vec-digit-pointer src)
               (bytes-for-digits written-length))))))
