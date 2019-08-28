;;;; mpz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype sign ()
  "The sign of an integer."
  '(member 1 -1))

(deftype intermediate ()
  "An intermediate computation with a digit."
  'fixnum)

;;; Digit storage

(defun make-storage (n)
  (assert (plusp n))
  (make-array n :element-type 'digit :initial-element 0))

;;; The MPZ type

(defclass mpz ()
  ((storage :initarg :storage
           :reader storage)
   (sign :initarg :sign
         :reader sign)))

;;; Conversion functions

(defun mpz-integer (mpz)
  (check-type mpz mpz)
  (* (sign mpz)
     (loop :for digit :across (storage mpz)
           :for b := 1 :then (* b $base)
           :sum (* digit b))))

(defun integer-mpz (n)
  (check-type n integer)
  (cond
    ((zerop n)
     (load-time-value
       (make-instance 'mpz :sign 1 :storage (make-storage 1))))
    ((= 1 n)
     (load-time-value
      (make-instance 'mpz :sign 1 :storage (let ((s (make-storage 1)))
                                             (setf (aref s 0) 1)
                                             s))))
    (t
     (let ((sign (if (minusp n) -1 1)))
       (setf n (abs n))
       (loop :until (zerop n)
             :collect (multiple-value-bind (quo rem) (floor n $base)
                        (setf n quo)
                        rem)
               :into digits
             :finally (return (let ((storage (make-storage (length digits))))
                                (map-into storage #'identity digits)
                                (make-instance 'mpz
                                               :sign sign
                                               :storage storage))))))))
(defun mpz-size (mpz)
  "How many digits does the MPZ have?

If MPZ is equal to 0, then this is 0."
  (1+ (or (position-if-not #'zerop (storage mpz) :from-end t)
          0)))

(defun mpz-integer-length (mpz)
  "How many bits are needed to represent MPZ in two's complement?"
  (let ((size (mpz-size mpz)))
    (if (zerop size)
        0
        (+ (* $digit-bits (1- size))
           (integer-length (aref (storage mpz) (1- size)))
           (/ (1- (sign mpz)) 2)))))

(defun mpz-zerop (mpz)
  (every #'zerop (storage mpz)))

(defun mpz-plusp (mpz)
  (and (= 1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-minusp (mpz)
  (and (= -1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-abs (mpz)
  (make-instance 'mpz :sign 1
                      :storage (storage mpz)))

(defun mpz-negate (mpz)
  (make-instance 'mpz :sign (- (sign mpz))
                      :storage (storage mpz)))

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

(defun %mpz-+ (a b)
  (assert (= 1 (sign a) (sign b)))
  (flet ((plus (a size-a b size-b)
           ;; size-a >= size-b
           (let* ((r (make-storage (1+ size-a)))
                  (carry 0))
             (loop :for i :below size-b
                   :for ai := (aref (storage a) i)
                   :for bi := (aref (storage b) i)
                   :for d := (+ ai bi carry)
                   :do (cond
                         ((>= d $base)
                          (decf d $base)
                          (setf carry 1))
                         (t
                          (setf carry 0)))
                       (setf (aref r i) d))
             (loop :for i :from size-b :below size-a
                   :for ai := (aref (storage a) i)
                   :for d := (+ ai carry)
                   :do (cond
                         ((>= d $base)
                          (decf d $base)
                          (setf carry 1))
                         (t
                          (setf carry 0)))
                       (setf (aref r i) d))
             (setf (aref r size-a) carry)
             (make-instance 'mpz :sign 1 :storage r))))
    (let ((size-a (mpz-size a))
          (size-b (mpz-size b)))
      (if (>= size-a size-b)
          (plus a size-a b size-b)
          (plus b size-b a size-a)))))

(defun %mpz-- (a b)
  (assert (= 1 (sign a) (sign b)))
  (flet ((minus (a b)
           ;; a > b > 0
           (let* ((size-a (mpz-size a))
                  (size-b (mpz-size b))
                  (r (make-storage size-a))
                  (carry 0))
             (assert (>= size-a size-b))
             (loop :for i :below size-b
                   :for ai := (aref (storage a) i)
                   :for bi := (aref (storage b) i)
                   :for d := (- ai bi carry)
                   :do (cond
                         ((minusp d)
                          (incf d $base)
                          (setf carry 1))
                         (t
                          (setf carry 0)))
                       (setf (aref r i) d))
             (loop :for i :from size-b :below size-a
                   :for ai := (aref (storage a) i)
                   :for d := (- ai carry)
                   :do (cond
                         ((minusp d)
                          (incf d $base)
                          (setf carry 1))
                         (t
                          (setf carry 0)))
                       (setf (aref r i) d))
             (assert (zerop carry))
             (make-instance 'mpz :sign 1 :storage r))))
    (cond
      ((mpz-= a b)
       (integer-mpz 0))
      ((mpz-> a b)
       (minus a b))
      (t
       (mpz-negate (minus b a))))))

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

