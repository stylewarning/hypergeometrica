;;;; mpd-reciprocal.lisp
;;;;
;;;; Copyright (c) 2022-2023 Robert Smith

(in-package #:hypergeometrica)

(defun estimate-reciprocal (a)
  (assert (not (mpz-zerop (mantissa a))))
  (let ((m (mantissa a))
        (e (exponent a)))
    (let* ((size (mpz-size m))
           (msb (with-vec (m m_)
                  (m_ (1- size)))))
      ;; 1/(a*2^e)
      ;; = (1/a)2^(-e)
      ;; ~ sign(a) * (1/msb(|a|) * 1/2^(len(a))) * 2^(-e)
      ;; ~ sign(a) * 1/msb(|a|) * 2^(-n-len(a))
      (let ((recip-mantissa-estimate
              (integer-mpz (round $base msb) 'mpz/ram)))
        (when (mpz-minusp m)
          (mpz-negate! recip-mantissa-estimate))
        (make-instance 'mpd
          :mantissa recip-mantissa-estimate
          :exponent (+ (- e)
                       (- size)))))))

(defun mpd-reciprocal (a bits)
  ;; Newton's method on f(x) = 1/x - a.
  ;;
  ;; Ordinarily the iteration is:
  ;;
  ;;     x(n+1) = 2*x(n) - a*x(n)^2
  ;;
  ;; We rewrite it by introducing a term:
  ;;
  ;;     h(n) = 1 - a*x(n)
  ;;
  ;;     x(n+1) = x(n) + x(n)*h(n).
  ;;
  ;; Since h(n) tends to zero as n tends to infinity, it serves as a
  ;; good estimate for the accuracy of x(n), allowing us to
  ;; confidently terminate the iteration.
  (loop :with digits := (max 1 (ceiling bits $digit-bits))
        :with x := (estimate-reciprocal a)
        :for iter-number :from 1
        :do (let ((h (mpd-- (integer-mpd 1)
                            (mpd-* a x))))
              (multiple-value-bind (zeros zero?) (mpd-oom h)
                (setf zeros (- zeros))  ; Want a positive number.
                (cond
                  ;; Did we converge exactly?
                  (zero?
                   (loop-finish))
                  ;; Have we exceeded the desired precision?
                  ((>= zeros digits)
                   ;; TODO: Truncate?
                   (loop-finish))
                  ;; We haven't reached out desired precision. We'll
                  ;; need to do another iteration.
                  (t
                   (when *verbose*
                     (format t "~&MPD-RECIPROCAL: iter=~D : ~D/~D digit~:P, h = ~A, est size = ~A~%"
                             iter-number
                             zeros
                             digits
                             (mpd-mpfr h)
                             (vec-digit-length (storage (mantissa x)))))
                   
                   ;; TODO: Should we truncate?
                  
                   ;; Perform the next iteration on x.
                   (setf x (mpd-+ x (mpd-* x h)))))))
        :finally (return x)))


(defun test-recip (digits &optional (v 3))
  (let ((bits (ceiling (* digits (log 10.0d0 2.0d0)))))
    (sb-mpfr:set-precision (max (+ 8 bits) 250))
    (let ((x (mpd-reciprocal (integer-mpd v) bits)))
      (format t "~D digit~:P 1/~D = ~A~%" digits v (mpd-mpfr x)))))

