;;;; mpz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype sign ()
  "The sign of an integer."
  '(member 1 -1))

;;; Digit storage

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

(defun resize-storage (a n)
  "Adjust the length of the storage A by N elements."
  (declare (type storage a)
           (type alexandria:array-length n))
  (unless (= n (length a))
    (adjust-array a n :initial-element 0))
  (values))

(defun resize-storage-by (a delta)
  "Adjust the length of the storage A by DELTA elements."
  (declare (type storage a)
           (type fixnum delta))
  (let ((new-length (+ delta (length a))))
    (assert (plusp new-length))
    (resize-storage a new-length)))


;;; The MPZ type

(defstruct (mpz (:conc-name nil)
                (:predicate mpz?)
                (:copier nil)
                (:constructor make-mpz (sign storage)))
  (sign 1 :type sign :read-only t)
  (storage (make-storage 0) :type storage :read-only t))
#+sbcl (declaim (sb-ext:freeze-type mpz))

(declaim (ftype (function (mpz) raw-storage) raw-storage))
(defun raw-storage (mpz)
  (raw-storage-of-storage (storage mpz)))

;;; Conversion functions

(defun mpz-integer (mpz)
  (declare (type mpz mpz))
  (* (sign mpz)
     (loop :for digit :across (storage mpz)
           :for b := 1 :then (* b $base)
           :sum (* digit b))))

(defun integer-mpz (n)
  (declare (type integer n))
  (cond
    ((zerop n)
     (make-mpz 1 (make-storage 1)))
    ((= 1 n)
     (make-mpz 1 (let ((s (make-storage 1)))
                   (setf (aref s 0) 1)
                   s)))
    (t
     (let ((sign (if (minusp n) -1 1)))
       (setf n (abs n))
       (loop :until (zerop n)
             :collect (multiple-value-bind (quo rem) (floor n $base)
                        (setf n quo)
                        rem)
               :into digits
             :finally (return (let ((storage (make-storage (length digits))))
                                (replace storage digits)
                                (make-mpz sign storage))))))))

;;; Size, Length, etc.

(defun mpz-size (mpz)
  "How many digits does the MPZ have?

If MPZ is equal to 0, then this is 0."
  (1+ (or (position-if-not #'zerop (storage mpz) :from-end t)
          0)))

(defun mpz-uses-minimal-storage-p (mpz)
  (= (length (storage mpz)) (mpz-size mpz)))

(defun optimize-storage (mpz)
  (resize-storage (storage mpz) (mpz-size mpz))
  mpz)

(defun mpz-integer-length (mpz)
  "How many bits are needed to represent MPZ in two's complement?"
  (let ((size (mpz-size mpz)))
    (if (zerop size)
        0
        (+ (* $digit-bits (1- size))
           (integer-length (aref (storage mpz) (1- size)))
           (/ (1- (sign mpz)) 2)))))

(defmethod print-object ((mpz mpz) stream)
  (print-unreadable-object (mpz stream :type t :identity t)
    (format stream "~D bit~:P~:[~;*~]"
            (mpz-integer-length mpz)
            (mpz-uses-minimal-storage-p mpz))))


;;; Comparison functions

(defun mpz-zerop (mpz)
  (every #'zerop (storage mpz)))

(defun mpz-plusp (mpz)
  (and (= 1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-minusp (mpz)
  (and (= -1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-abs (mpz)
  (make-mpz 1 (storage mpz)))

(defun mpz-negate (mpz)
  (make-mpz (- (sign mpz)) (storage mpz)))

(defun mpz-= (a b)
  (and (= (sign a) (sign b))
       (= (mpz-size a) (mpz-size b))
       (every #'= (storage a) (storage b))))

(defun mpz-/= (a b)
  (not (mpz-= a b)))

(defun %mpz-> (a b)
  (assert (= 1 (sign a) (sign b)))
  (let ((size-a (mpz-size a))
        (size-b (mpz-size b)))
    (if (/= size-a size-b)
        (> size-a size-b)
        (loop :for i :from (1- size-a) :downto 0
              :for ai := (aref (storage a) i)
              :for bi := (aref (storage b) i)
              :do (cond
                    ((> ai bi) (return t))
                    ((< ai bi) (return nil)))
              :finally (return nil)))))

(defun mpz-> (a b)
  (cond
    ((> (sign a) (sign b)) t)
    ((< (sign a) (sign b)) nil)
    ;; Now we know they have the same sign. If they're both negative,
    ;; then reverse.
    ((= -1 (sign a))
     (%mpz-> (mpz-abs b) (mpz-abs a)))
    ;; They have the same sign and they're positive.
    (t
     (%mpz-> a b))))

(defun mpz->= (a b)
  (or (mpz-= a b)
      (mpz-> a b)))

(defun mpz-< (a b)
  (not (mpz->= a b)))

(defun mpz-<= (a b)
  (not (mpz-> a b)))

(defun %add-storages/unsafe (a size-a b size-b)
  (declare (type raw-storage a b)
           (type alexandria:array-length size-a size-b))
  ;; size-a >= size-b
  (let* ((r     (make-storage (1+ size-a)))
         (raw   (raw-storage-of-storage r))
         (carry 0))
    (loop :for i :below size-b
          :for ai := (aref a i)
          :for bi := (aref b i)
          :for d :of-type intermediate := (+ ai bi carry)
          :do (cond
                ((>= d $base)
                 (decf d $base)
                 (setf carry 1))
                (t
                 (setf carry 0)))
              (setf (aref raw i) d))
    (loop :for i :from size-b :below size-a
          :for ai := (aref a i)
          :for d :of-type intermediate := (+ ai carry)
          :do (cond
                ((>= d $base)
                 (decf d $base)
                 (setf carry 1))
                (t
                 (setf carry 0)))
              (setf (aref raw i) d))
    ;; Account for the carry.
    (cond
      ((zerop carry) (resize-storage-by r -1))
      (t             (setf (aref raw size-a) carry)))
    ;; Return the storage
    r))

(defun %mpz-+ (a b)
  (assert (= 1 (sign a) (sign b)))
  (let ((size-a (mpz-size a))
        (size-b (mpz-size b)))
    (if (>= size-a size-b)
        (make-mpz 1 (%add-storages/unsafe (raw-storage a) size-a
                                          (raw-storage b) size-b))
        (make-mpz 1 (%add-storages/unsafe (raw-storage b) size-b
                                          (raw-storage a) size-a)))))

(defun %subtract-storages/unsafe (a size-a b size-b)
  (declare (type raw-storage a b)
           (type alexandria:array-length size-a size-b))
  ;; a > b > 0
  (assert (>= size-a size-b))
  (let* ((r (make-storage size-a))
         (raw (raw-storage-of-storage r))
         (carry 0))
    (declare (type bit carry))
    (loop :for i :below size-b
          :for ai := (aref a i)
          :for bi := (aref b i)
          :for d :of-type intermediate := (- ai bi carry)
          :do (cond
                ((minusp d)
                 (incf d $base)
                 (setf carry 1))
                (t
                 (setf carry 0)))
              (setf (aref raw i) d))
    (loop :for i :from size-b :below size-a
          :for ai := (aref a i)
          :for d :of-type intermediate := (- ai carry)
          :do (cond
                ((minusp d)
                 (incf d $base)
                 (setf carry 1))
                (t
                 (setf carry 0)))
              (setf (aref raw i) d))
    (assert (zerop carry))
    r))

(defun %mpz-- (a b)
  (assert (= 1 (sign a) (sign b)))
  (cond
    ((mpz-= a b)
     (integer-mpz 0))
    ((mpz-> a b)
     (optimize-storage
      (make-mpz 1 (%subtract-storages/unsafe (raw-storage a) (mpz-size a)
                                             (raw-storage b) (mpz-size b)))))
    (t
     (optimize-storage
      (make-mpz -1 (%subtract-storages/unsafe (raw-storage b) (mpz-size b)
                                              (raw-storage a) (mpz-size a)))))))

(defun mpz-+ (a b)
  (cond
    ((mpz-zerop a) b)
    ((mpz-zerop b) a)
    ((= 1 (sign a) (sign b))
     (%mpz-+ a b))
    ((= -1 (sign a) (sign b))
     (mpz-negate (%mpz-+ (mpz-abs a) (mpz-abs b))))
    ;; a > 0, b < 0
    ((= -1 (sign b))
     (%mpz-- a (mpz-abs b)))
    ;; a < 0, b > 0
    (t
     (%mpz-- b (mpz-abs a)))))

(defun mpz-- (a b)
  (mpz-+ a (mpz-negate b)))

