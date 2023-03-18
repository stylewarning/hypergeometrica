;;;; divrem.lisp
;;;;
;;;; Copyright (c) 2022-2023 Robert Smith

(in-package #:hypergeometrica)

;;; We do integer division using MPDs.
;;;
;;; N.B. We could also compute floor(a/b) as
;;;
;;;     a * ceiling(2^k/b) >> k
;;;
;;; where a*b <= 2^k. This would allow pure integer operations, and
;;; may also be more efficient, but it would lead to some code
;;; duplication.

(defun mpz-divrem (a b)
    "Compute the quotient and remainder of the integer division of A and B.

If (Q, R) = mpz-divrem(A, B), then A = Q*B + R."
  (let* ((abs-a (mpz-abs a))
         (abs-b (mpz-abs b))
         (q     (%mpz-div abs-a abs-b))
         (r    (mpz-- abs-a (mpz-* abs-b q))))
    ;; q may be off by one.
    (cond
      ((mpz-minusp r)
       (setf q (mpz-1- q)
             r (mpz-+ r abs-b)))
      ((mpz->= r abs-b)
       (setf q (mpz-1+ q)
             r (mpz-- r abs-b))))

    ;; Fix sign
    ;;
    ;; TODO: Sign of remainder?
    (when (minusp (* (sign a) (sign b)))
      (mpz-negate! q))

    ;; Return
    (values q r)))

(defun %mpz-div (a b)
  (cond
    ((mpz-zerop b)
     (error "Cannot divide by zero."))
    ((mpz-< a b)
     (integer-mpz 0 'mpz/ram))
    ((mpz-= a b)
     (integer-mpz 1 'mpz/ram))
    ;; TODO: add branch for size-1 / size-1 division
    ;;
    ;; TODO: more testing of the below algorithm...
    (t
     ;; Note that due to the use of Newton's method, we might actually
     ;; do a division that has a non-terminating decimal expansion,
     ;; and as such could be an under-estimate. We will fix this up in
     ;; the call above.
     (let* ((bits-needed (+ (* 2 $digit-bits) ; slack
                            (- (mpz-bit-size a)
                               (mpz-bit-size b))))
            (a (mpz-mpd a))
            (b (mpz-mpd b)))
       (mpd-integer-part (mpd-* a (mpd-reciprocal b bits-needed)))))))
