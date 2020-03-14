;;; fixed-width-arithmetic.lisp
;;;
;;; Copyright (c) 2014-2019 Robert Smith

(in-package #:hypergeometrica)

;;;;;;;;;;;;;;;;;;;;;;; Fixed-Width Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline split-byte))
(defun split-byte (x bits)
  "Split the non-negative integer X into two values X0 and X1 such that

    X = X1 << bits + X0."
  (values (ldb (byte bits 0) x)
          (ash x (- bits))))

(declaim (inline join-bytes))
(defun join-bytes (x0 x1 bits)
  "Join the bytes X0 and X1 into a value X such that

    X = X1 << bits + X0.

Ideally BITS is greater than the size of X0."
  (+ x0 (ash x1 bits)))

(defun fixed-width-add (a b width &aux (width/2 (ash width -1)))
  "Add the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not exceed WIDTH bits.

Return two values S0 and S1 such that

    A+B = S0 + S1 << WIDTH."
  (multiple-value-bind (a0 a1) (split-byte a width/2)
    (multiple-value-bind (b0 b1) (split-byte b width/2)
      (multiple-value-bind (low carry) (split-byte (+ a0 b0) width/2)
        (multiple-value-bind (high carry) (split-byte (+ carry a1 b1) width/2)
          (values (join-bytes low high width/2) carry))))))

(defun fixed-width-multiply (a b width &aux (width/2 (ash width -1)))
  "Multiply the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not wxceed WIDTH bits.

Return two values P0 and P1 such that

    A*B = P0 + P1 << WIDTH."
  ;; Split operands into half-width components.
  (multiple-value-bind (a0 a1) (split-byte a width/2)
    (multiple-value-bind (b0 b1) (split-byte b width/2)
      ;; Compute partial products. If W = 2^WIDTH and W' = W/2, then
      ;;
      ;;   A   = A0 + A1*W'
      ;;   B   = B0 + B1*W'
      ;;
      ;;   A*B = (A0 + A1*W')*(B0 + B1*W')
      ;;       = A0*B0 + (A0*B1 + A1*B0)*W' + A1*B1*W
      ;;
      ;; Each of these sub-A*B products are of width WIDTH, and are
      ;; broken into half-width components as above, except for the
      ;; product C3 = A1*B1.
      (multiple-value-bind (c0-lo c0-hi) (split-byte (* a0 b0) width/2)
        (multiple-value-bind (c1a-lo c1a-hi) (split-byte (* a0 b1) width/2)
          (multiple-value-bind (c1b-lo c1b-hi) (split-byte (* a1 b0) width/2)
            (let ((c3 (* a1 b1)))
              ;; Compute columns and carries as in longhand
              ;; multiplication. Each column tracks WIDTH/2 bits.
              ;;
              ;; Column 0   = C0-LO
              ;; Column 1   = C0-HI + C1A-LO + C1B-LO
              ;; Column 2,3 = C1A-HI + C1B-HI + C3 + COL1-CARRY
              (multiple-value-bind (col11 col1-carry1) (fixed-width-add c1a-lo c1b-lo width/2)
                (multiple-value-bind (col1 col1-carry2) (fixed-width-add col11 c0-hi width/2)
                  (let ((col1-carry (+ col1-carry1 col1-carry2)))
                    (values (join-bytes c0-lo col1 width/2)
                            (+ c1a-hi
                               c1b-hi
                               c3
                               col1-carry))))))))))))
