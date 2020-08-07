;;;; mpf.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:hypergeometrica)

(define-symbolic-enumeration rounding-mode
  rnd/n                                 ; Round to nearest, ties with even.
  rnd/z                                 ; Round toward zero.
  rnd/d                                 ; Round down, toward -inf.
  rnd/u                                 ; Round up, toward +inf.
  rnd/na                                ; Round to nearest, ties away from zero.
  rnd/a                                 ; Round away from zero.
  rnd/f)                                ; Faithful rounding

(defconstant $expn-zero (minimum-signed-byte 64))
(defconstant $expn-inf (1- (maximum-signed-byte 64)))
(defconstant $expn-nan (maximum-signed-byte 64))

(defstruct (mpf (:predicate mpf?)
                (:copier nil))
  (sign 1                   :type sign)
  (expn $expn-zero          :type (signed-byte 64))
  (storage (make-storage 0) :type storage :read-only t))

(defun mpf-digit (mpf index)
  (if (>= index (length (storage mpf)))
      0
      (aref (storage mpf) index)))

(declaim (ftype (function (mpf) alexandria:array-length) mpf-size))
(defun mpf-size (mpf)
  "How many digits does the mantissa of MPF have?"
  (declare (optimize speed (safety 0) (debug 0)))
  (loop :with raw := (raw-storage-of-storage (mpf-storage mpf))
        :for i :from (1- (length raw)) :downto 0
        :unless (zerop (aref raw i))
          :do (return (1+ i))
        :finally (return 0)))

(defun mpf-finite? (mpf)
  (> $expn-inf (mpf-expn mpf)))

(defun mpf-infinite? (mpf)
  (= $expn-inf (mpf-expn mpf)))

(defun make-mpf-positive-infinity ()
  (make-mpf :sign 1
            :expn $expn-inf
            :storage (make-storage 0)))

(defun make-mpf-negative-infinity ()
  (make-mpf :sign -1
            :expn $expn-inf
            :storage (make-storage 0)))

(defun mpf-nan? (mpf)
  (= $expn-nan (mpf-expn mpf)))

(defun mpf-nan! (mpf)
  "Overwrite MPF to be a NaN value."
  (resize-storage (mpf-storage mpf) 0)
  (setf (mpf-expn mpf) $expn-nan
        (mpf-sign mpf) 1)
  nil)

(defun make-mpf-nan ()
  (make-mpf :sign 1
            :expn $expn-nan
            :storage (make-storage 0)))

(defun mpf-zerop (mpf)
  (= $expn-zero (mpf-expn mpf)))

(defun mpf-zero! (mpf)
  "Overwrite MPF to be zero."
  (resize-storage (mpf-storage mpf) 0)
  (setf (mpf-expn mpf) $expn-zero
        (mpf-sign mpf) 1)
  nil)

(defun make-mpf-zero ()
  "Make an MPF value equal to zero."
  (make-mpf :sign 1
            :expn $expn-zero
            :storage (make-storage 0)))

(defun mpf-integralp (mpf)
  "Does the MPF represent an integral (i.e., integer) value?"
  (and (mpf-finite? mpf)
       (or (mpf-zerop mpf)
           (not (minusp (mpf-expn mpf))))))

(defun mpf-fractionalp (mpf)
  "Does the MPF represent a fractional (i.e., non-integer) value?"
  (and (mpf-finite? mpf)
       (not (mpf-zerop mpf))
       (minusp (mpf-expn mpf))))

(defun mpf-plusp (mpf)
  (and (not (mpf-nan? mpf))
       (not (mpf-zerop mpf))
       (= 1 (mpf-sign mpf))))

(defun mpf-minusp (mpf)
  (and (not (mpf-nan? mpf))
       (not (mpf-zerop mpf))
       (= -1 (mpf-sign mpf))))

(defun copy-mpf (mpf)
  (make-mpf :sign (mpf-sign mpf)
            :expn (mpf-expn mpf)
            :storage (copy-storage (mpf-storage mpf))))

(defun mpf-mantissa-bits (mpf)
  "What is the minimum number of bits needed to represent the mantissa of MPF?"
  (let ((size (mpf-size mpf)))
    (if (zerop size)
        0
        (+ (* $digit-bits (1- size))
           (integer-length (aref (mpf-storage mpf) (1- size)))))))

(defun mpf-negate! (mpf)
  (setf (mpf-sign mpf) (- (mpf-sign mpf)))
  nil)

(defun mpf-negate (mpf)
  (let ((mpf (copy-mpf mpf)))
    (mpf-negate! mpf)
    mpf))

(defun integer-mpf (n)
  (declare (type integer n))
  (cond
    ((zerop n)
     (make-mpf-zero))
    (t
     (let ((sign (signum n)))
       (setf n (abs n))
       (let* ((num-bits   (integer-length n))
              (zeroes     (count-trailing-zeroes n))
              (num-digits (ceiling (- num-bits zeroes) $digit-bits))
              (storage    (make-storage num-digits)))
         (loop :for i :from 0
               :for offset :from zeroes :below num-bits :by $digit-bits
               :do (setf (storage-ref storage i) (ldb (byte $digit-bits offset) n)))
         (make-mpf :sign sign
                   :expn zeroes
                   :storage storage))))))

(defmethod print-object ((mpf mpf) stream)
  (print-unreadable-object (mpf stream :type t :identity t)
    (cond
      ((mpf-nan? mpf) (format stream "NaN"))
      ((mpf-infinite? mpf) (format stream "~:[-~;+~]Infinity" (mpf-plusp mpf)))
      (t
       (format stream "~:[Frac~;Int~] (~D bit~:P)"
               (mpf-integralp mpf)
               (mpf-mantissa-bits mpf))))))

(define-symbolic-enumeration comparison
  cmp/in                                ; INcomparable
  cmp/gt                                ; Greater Than
  cmp/lt                                ; Less Than
  cmp/eq)                               ; EQual to

(defun mpf-absolute-compare (a b)
  "Compare the absolute values of A and B. (NaN == NaN is true here.)"
  (cond
    ((/= (mpf-expn a) (mpf-expn b))     ; Checks finiteness too.
     (if (< (mpf-expn a) (mpf-expn b))
         cmp/lt
         cmp/gt))
    (t
     (let ((len (max (mpf-size a) (mpf-size b))))
       (loop :for i :from (1- len) :downto 0 :do
         (let ((ai (mpf-digit a i))
               (bi (mpf-digit b i)))
           (when (/= ai bi)
             (return-from mpf-absolute-compare
               (if (< ai bi)
                   cmp/lt
                   cmp/gt)))))
       ;; If we made it here, they're equal.
       cmp/eq))))

(defun mpf-compare (a b)
  (cond
    ((or (mpf-nan? a) (mpf-nan? b))
     cmp/in)
    ((/= (mpf-sign a) (mpf-sign b))
     (if (and (mpf-zerop a) (mpf-zerop b))
         cmp/eq
         (if (plusp (mpf-sign a))
             cmp/gt
             cmp/lt)))
    (t
     (ecase (mpf-absolute-compare a b)
       (cmp/lt (if (= -1 (mpf-sign a)) cmp/gt cmp/lt))
       (cmp/gt (if (= -1 (mpf-sign a)) cmp/lt cmp/gt))
       (cmp/eq cmp/eq)))))
