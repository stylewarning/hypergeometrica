;;;; mpd-sqrt.lisp
;;;;
;;;; Copyright (c) 2022-2023 Robert Smith

(in-package #:hypergeometrica)

(defun estimate-inv-sqrt (a)
  (let ((m (mantissa a))
        (e (exponent a)))
    (assert (not (mpz-minusp m)))
    (let* ((size (mpz-size m))
           (msb (with-vec (m m_)
                  (m_ (1- size)))))
      ;; 1/sqrt(a*B^e)
      ;; = 1/sqrt(a) * 1/sqrt(B^e)
      ;; = 1/sqrt(a) * B^(-e/2)
      ;; ~ 1/sqrt(msb(a) * B^(len(a) - 1)) * B^(-e/2)
      ;; = 1/sqrt(msb(a)) * 1/B^((len(a)-1)/2) * B^(-e/2)
      ;; = 1/sqrt(msb(a)) * B^([1 - len(a) - e]/2)
      ;; = B/sqrt(msb(a)) * B^([-1 - len(a) - e]/2)
      ;;
      ;; we make sure the B^*
      ;; exponent is even so that we
      ;; can divide by two
      ;; perfectly.
      (let* ((double-new-exp (+ -1 (- size) (- e)))
             (inv-sqrt-mantissa-estimate
               (if (oddp double-new-exp)
                   (integer-mpz (round $base (sqrt (* $base msb))) 'mpz/ram)
                   (integer-mpz (round $base (sqrt msb)) 'mpz/ram))))
        (when (oddp double-new-exp)
          (incf double-new-exp))
        (assert (evenp double-new-exp))
        (when (mpz-minusp m)
          (mpz-negate! inv-sqrt-mantissa-estimate))
        (make-instance 'mpd
          :mantissa inv-sqrt-mantissa-estimate
          :exponent (/ double-new-exp 2))))))

(defun mpd-inv-sqrt (a bits)
  ;; Newton's iteration for f(x) = 1/x^2 - a.
  ;;
  ;; This is ordinarily
  ;;
  ;;     x(n+1) = (3*x(n) - A*x(n)^3)/2
  ;;
  ;; but as with the reciprocal formula, we write it
  ;;
  ;;     h(n) = 1 - a*x(n)^2
  ;;
  ;;     x(n+1) = [2*x(n) + x(n)*h(n)]/2
  ;;
  ;; and estimate the error by the cancellation of h(n).
  (loop :with digits := (max 1 (ceiling bits $digit-bits))
        :with x := (estimate-inv-sqrt a)
        :for iter-number :from 1
        :do (let ((h (mpd-- (integer-mpd 1)
                            (mpd-* a (mpd-* x x)))))
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
                     (format t "~&MPD-INV-SQRT: iter=~D : ~D/~D digit~:P, h = ~A, est size = ~A~%"
                             iter-number
                             zeros
                             digits
                             (mpd-mpfr h)
                             (vec-digit-length (storage (mantissa x)))))

                   ;; TODO: Should we truncate?
                   
                   ;; Perform the next iteration.
                   (setf x (mpd-+
                            (mpd-+ x x)
                            (mpd-* x h)))
                   (mpd-half! x)))))
        :finally (return x)))

(defun mpd-sqrt (a bits)
  (mpd-* a (mpd-inv-sqrt a bits)))

(defun test-sqrt (digits &optional (v 2))
  (let ((bits (ceiling (* digits (log 10.0d0 2.0d0)))))
    (sb-mpfr:set-precision (max (+ 8 bits) 250))
    (let ((x (mpd-sqrt (integer-mpd v) bits)))
      (format t "~D digit~:P sqrt(~D) = ~A~%" digits v (mpd-mpfr x)))))

