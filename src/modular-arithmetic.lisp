;;;; modular-arithmetic.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;;;;;;;;;;;;;;;;;;;;;;;; Modular Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These two variables are for efficiency reasons only. They're
;;; changeable at compile-time, and affect the moduli that are found
;;; in #'DEFAULT-MODULI.
(defconstant +modulus-bits+ 63
  "The number of bits that each modulus must have.")
(defconstant +lg-modulus+ (1- +modulus-bits+))


(defconstant $max-modulus (1- $base))
(deftype modulus ()
  `(integer 2 ,$max-modulus))

;; These are NOTINLINE'd below.
(declaim (ftype (function (digit digit modulus) digit) m+ m- m*))
(declaim (inline  m+ m- m* m/ m1+ m1- negate-mod inv-mod expt-mod))

(defun m- (a b m)
  "Compute A - B (mod M).

Assumes 0 <= A,B < M."
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (if (< a b)
      (the digit (+ (the digit (- m b)) a))
      (the digit (- a b))))

(defun m+ (a b m)
  "Compute A + B (mod M).

Assumes 0 <= A,B < M."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (inline m-))
  (if (zerop b)
      a
      (m- a (the digit (- m b)) m)))

(defun m1+ (a m)
  "Increment A modulo M.

Assumes 0 <= A < M."
  (let ((a (1+ a)))
    (if (= a m)
        0
        a)))

(defun m1- (a m)
  "Decrement A modulo M.

Assumes 0 <= A < M."
  (if (zerop a)
      (1- m)
      (1- a)))

(defun negate-mod (a m)
  "Negate A modulo M.

Assumes 0 <= A < M."
  (if (zerop a)
      0
      (- m a)))

(defun m* (a b m)
  (declare (type modulus m)
           (type digit a b))
  #+sbcl
  (multiple-value-bind (lo hi) (mul128 a b)
    (the digit (nth-value 1 (div128 lo hi m))))
  #-sbcl
  (mod (* a b) m))

(defun inv-mod (x m)
  "Compute X^-1 (mod M)."
  (labels ((egcd (x b a u)
             (if (zerop x)
                 (if (= 1 b)
                     (mod a m)
                     (error "~D is not invertible in Z/~DZ" x m)) ; X divides M
                 (multiple-value-bind (q r) (floor b x)
                   (egcd r x u (- a (* u q)))))))
    (egcd x m 0 1)))

(defun inv-mod/unsafe (x m)
  "Compute X^-1 (mod M). Assumes X is invertible."
  (declare (type digit x)
           (type modulus m)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (labels ((egcd (x b a u)
             (declare (type digit b a u x))
             (if (zerop x)
                 (mod a m)
                 (multiple-value-bind (q r) (floor b x)
                   (egcd r x u (the digit (- a (the digit (* u q)))))))))
    (egcd x m 0 1)))

(defun m/ (a b m)
  "Compute A / B = A * B^-1 (mod M)."
  (m* a (inv-mod b m) m))

(defun expt-mod (a n m)
  "Compute A ^ N (mod M) for integer N."
  (declare (type digit a)
           (type alexandria:non-negative-fixnum n)
           (type modulus m)
           (inline m*)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((result 1))
    (declare (type digit result))
    (loop
      (when (oddp n)
        (setf result (m* result a m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from expt-mod result))
      (setf a (m* a a m)))))

(declaim (inline expt-mod/2^n))
(defun expt-mod/2^n (a n m)
  "Compute A ^ (2 ^ N) (mod M) for integer N."
  (declare (type digit a)
           (type alexandria:non-negative-fixnum n)
           (type modulus m)
           (inline m*)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (dotimes (i n a)
    (setf a (m* a a m))))

(defun expt-mod/safe (a n m)
  "Compute A ^ N (mod M) for integer N."
  (when (minusp n)
    (setf a (inv-mod a m)
          n (- n)))

  (let ((result 1))
    (loop
      (when (oddp n)
        (setf result (mod (* result a) m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from expt-mod/safe result))
      (setf a (mod (* a a) m)))))

(declaim (notinline m+ m- m* m/ m1+ m1- negate-mod inv-mod expt-mod))

;;; Mega-Fast Multiplication

;;; Has a precondition that  < 2^(64 + min-modulus-length)
(declaim (inline mod128/fast m*/fast))
(defun mod128/fast (lo hi m m-inv)
  "Reduce

    lo + hi*2^64 (mod m)

using m and its inverse m-inv."
  (declare (type (unsigned-byte 64) lo hi m m-inv)
           (optimize speed (safety 0) (debug 0) (space 0)))
  #+hypergeometrica-safe
  (assert (or (zerop hi) (< (lg hi) +lg-modulus+)))
  (let* ((a1 (dpb (ldb (byte +lg-modulus+ 0) hi)
                  (byte +lg-modulus+ (- 64 +lg-modulus+))
                  (ash lo (- +lg-modulus+))))
         (q (nth-value 1 (mul128 a1 m-inv))))
    ;; r = r - q*m - m*2
    (multiple-value-bind (slo shi) (mul128 q m)
      (multiple-value-setq (lo hi) (sub128 lo hi slo shi))
      ;; Note that 2*M will always fit in 64 bits.
      (multiple-value-setq (lo hi) (sub128 lo hi (fx* 2 m) 0)))

    (multiple-value-setq (lo hi) (add128 lo hi (logand m (ub64/2 hi)) 0))
    (fx+ lo (logand m hi))))

(defun m*/fast (a b m m-inv)
  "Reduce

    a*b (mod m)

using m and its inverse m-inv."
  (declare (type (unsigned-byte 64) a b m m-inv)
           (optimize speed (safety 0) (debug 0) (space 0)))
  ;;(assert (= (* a b) (+ lo (ash hi 64))))
  (multiple-value-bind (lo hi) (mul128 a b)
    (mod128/fast lo hi m m-inv)))

;;; B and its companion B-INV is intended to be calculated once, which
;;; affords you fast and easy modular arithmetic.
(declaim (inline calc-b-inv))
(defun calc-b-inv (b m)
  (nth-value 0 (div128 0 b m)))

(declaim (inline m*/fast2-unreduced m*/fast2))
(defun m*/fast2-unreduced (b b-inv a m)
  (declare (type (unsigned-byte 64) a b m b-inv)
           (optimize speed (safety 0) (debug 0) (space 0)))
  #+hypergeometrica-paranoid
  (assert (<= 0 b (1- m)))
  (let* ((q (nth-value 1 (mul128 a b-inv))))
    (declare (type (unsigned-byte 64) q))
    (fx- (fx* a b) (fx* q m))))

(defun m*/fast2 (b b-inv a m)
  (declare (type (unsigned-byte 64) a b m b-inv)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((r (m*/fast2-unreduced b b-inv a m)))
    (declare (type (unsigned-byte 64) r))
    (when (>= r m)
      (decf r m))
    
    #+hypergeometrica-paranoid
    (assert (= (mod r m) (mod (* a b) m)))
    
    r))

