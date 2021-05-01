;;;; ntt.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

;;;;;;; Tests for parts of the NTT implementation ;;;;;;;
;; Tests for NTT multiplication itself are in tests/multiplication.lisp

;;;;;;; These are various NTT implementations used for testing ;;;;;;;

(defun ntt-forward-matrix (N m w)
  "Compute the NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N."
  (let ((matrix (make-array (list N N) :initial-element 1)))
    (loop :for row :from 0 :below N :do
      (loop :for col :from 0 :below N
            :for exponent := (* col row)
            :do (setf (aref matrix row col)
                      (h::expt-mod/safe w exponent m)))
          :finally (return matrix))))

(defun ntt-reverse-matrix (N m w)
  "Compute the inverse NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N.

This is just the conjugate-transpose of the NTT matrix, scaled by N."
  (labels ((conjugate-transpose (in)
             (let ((out (make-array (array-dimensions in))))
               (loop :for row :below (array-dimension out 0) :do
                 (loop :for col :below (array-dimension out 1) :do
                   (setf (aref out col row)
                         (h::m/ (h::inv-mod (aref in row col) m) N m))))
               out)))
    (conjugate-transpose (ntt-forward-matrix N m w))))

(defun matmul (A B modulus)
  "Multiply the matrices A and B over Z/mZ for modulus MODULUS."
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (l (array-dimension B 1))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop :for i :below m :do
      (loop :for k :below l :do
        (setf (aref C i k)
              (mod (loop :for j :below n
                         :sum (* (aref A i j)
                                 (aref B j k)))
                   modulus))))
    C))

(defun matvecmul (matrix v m)
  "Multiply the matrix MATRIX by the column vector V over Z/mZ."
  (let* ((N (length v))
         (result (copy-seq v)))
    (loop :for row :below N
          :do (setf (aref result row)
                    (loop :for col :below N
                          :for x := (aref matrix row col)
                          :for y := (aref v col)
                          :for x*y := (h::m* x y m)
                          :for s := x*y :then (h::m+ s x*y m)
                          :finally (return s)))
          :finally (return result))))

(defun ntt-forward-direct (in m w)
  "Compute the NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (h::expt-mod/safe w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (h::expt-mod/safe w^k j m)
                          :sum (h::m* w^jk (aref in j) m) :into s
                          :finally (return (mod s m))))
          :finally (return out))))

(defun ntt-reverse-direct (in m w)
  "Compute the inverse NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (setf w (h::inv-mod w m))
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (h::expt-mod w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (h::expt-mod/safe w^k j m)
                          :sum (* w^jk (aref in j)) :into s
                          :finally (return (h::m/ (mod s m) N m))))
          :finally (return out))))

;;; Tests start HERE!
(defun test-inversion/matrix (v m w)
  "Tests inversion property of matrix method."
  (let* ((N (length v))
         (eye (matmul
               (ntt-reverse-matrix N m w)
               (ntt-forward-matrix N m w)
               m)))
    (is (loop :for i :below N
              :always (loop :for j :below N
                            :always (= (aref eye i j)
                                       (if (= i j)
                                           1
                                           0)))))))

(defun test-inversion/direct (v m w)
  "Tests inversion property of the direct NTTs."
  (is (equalp v
              (ntt-reverse-direct (ntt-forward-direct v m w)
                                  m w))))

(defun test-inversion/ntt (v scheme i)
  "Tests inversion property of the fast NTTs."
  (is (equalp v
              (h::ntt-reverse (h::ntt-forward (copy-seq v) scheme i) scheme i))))


(deftest test-inversion-properties ()
  "Test that the forward and reverse transforms are actually inverses."
  (let ((N (expt 2 6)))
    (dolist (m (coerce (h::scheme-moduli h::**scheme**) 'list))
      (let* ((v (h::make-storage N))
             (scheme (h::make-modular-scheme (list m)))
             (w (aref (h::scheme-primitive-roots scheme) (h::lg N) 0 0 0)))
        (map-into v (lambda () (random m)))
        (test-inversion/matrix v m w)
        (test-inversion/direct v m w)
        (test-inversion/ntt v scheme 0)))))

(deftest test-ntt-from-various-definitions ()
  "Test that the NTTs agree in their transforms."
  (let ((N (expt 2 8)))
    (dolist (m (coerce (h::scheme-moduli h::**scheme**) 'list))
      (let* ((v (h::make-storage N))
             (scheme (h::make-modular-scheme (list m)))
             (w (aref (h::scheme-primitive-roots scheme) (h::lg N) 0 0 0)))
        (map-into v (lambda () (random m)))
        (let ((a (matvecmul (ntt-forward-matrix N m w) v m))
              (b (ntt-forward-direct v m w))
              (c (h::ntt-forward (copy-seq v) scheme 0)))
          (h::bit-reversed-permute! c)
          (is (equalp a b)))
        (let ((a (matvecmul (ntt-reverse-matrix N m w) v m))
              (b (ntt-reverse-direct v m w))
              (c (let ((v (copy-seq v)))
                   (h::bit-reversed-permute! v)
                   (h::ntt-reverse v scheme 0))))
          (is (equalp a b))
          (is (equalp b c)))))))
