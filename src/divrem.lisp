;;;; divrem.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

(defun mpz-div (a b)
  "Compute the truncated integer quotient of A and B. This will be precisely:

    sign(A)*sign(B)*floor(|A| / |B|).
"
  (let ((q (%mpz-div (mpz-abs a) (mpz-abs b))))
    (when (minusp (* (sign a) (sign b)))
      (mpz-negate! q))
    q))

(defun %mpz-div (a b)
  (cond
    ((mpz-zerop b)
     (error "Cannot divide by zero."))
    ((mpz-< a b)
     (integer-mpz 0 'mpz/ram))
    ((mpz-= a b)
     (integer-mpz 1 'mpz/ram))
    ;; TODO: add branch for size-1 / size-1 division
    ;; TODO: more testing of the below algorithm...
    (t
     (let* ((bits-needed (+ (* 2 $digit-bits) ; slack
                            (- (mpz-bit-size a)
                               (mpz-bit-size b))))
            (a (mpz-mpd a))
            (b (mpz-mpd b)))       
       (mpd-integer-part (mpd-* a (mpd-reciprocal b bits-needed)))))))

(defun mpz-divrem (a b)
  "Compute the quotient and remainder of the integer division of A and B.

If (Q, R) = mpz-divrem(A, B), then A = Q*B + R."
  (let ((q (mpz-div a b)))
    ;; TODO: negative numbers
    (values
     q
     (mpz-- a (mpz-* q b)))))
