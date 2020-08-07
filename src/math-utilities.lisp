;;;; math-utilities.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun power-of-two-p (n)
  "Is N a power-of-two?"
  (and (plusp n)
       (zerop (logand n (1- n)))))

(defun next-power-of-two (n)
  "Find the minimum K such that N <= 2^K."
  (if (power-of-two-p n)
      (1- (integer-length n))
      (integer-length n)))

(defun least-power-of-two->= (n)
  "What is the least power-of-two greater than or equal to N?"
  (if (power-of-two-p n)
      n
      (ash 1 (integer-length n))))

(declaim (inline lg))
(defun lg (n)
  (1- (integer-length n)))

(declaim (inline 2^))
(defun 2^ (n)
  (expt 2 n))

(defun count-trailing-zeroes (n)
  "Count the number of trailing zeros in the binary representation of the positive integer N."
  (assert (plusp n))
  (loop :for z :from 0
        :for x := n :then (ash x -1)
        :while (evenp x)
        :finally (return z)))

(defun coprimep (a b)
  "Are the integers A and B coprime?"
  (= 1 (gcd a b)))

(defun pairwise-coprimep (seq)
  "Are all integers in the sequence SEQ pairwise coprime?"
  (etypecase seq
    (list
     (loop :for m1 :on seq :do
       (loop :for m2 :in (rest m1) :do
         (unless (coprimep (first m1) m2)
           (return-from pairwise-coprimep nil)))))
    (vector
     (let ((len (length seq)))
       (loop :for i :below len :do
         (loop :for j :from (1+ i) :below len :do
           (unless (coprimep (aref seq i) (aref seq j))
             (return-from pairwise-coprimep nil)))))))
  ;; Otherwise, everything must be coprime.
  t)

(defun minimum-signed-byte (bits)
  (- (expt 2 (1- bits))))

(defun maximum-signed-byte (bits)
  (1- (expt 2 (1- bits))))

