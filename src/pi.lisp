;;;; pi.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:hypergeometrica)

;;;; Chudnovsky's Series for pi
;;;;
;;;; We actually calculate pi/10 so that the result is in [0,1).

(defconstant +chud-decimals-per-term+ (log 151931373056000 10d0))
(defconstant +chud-bits-per-term+ (log 151931373056000 2d0))
(defconstant +chud-a+ 13591409)
(defconstant +chud-b+ 545140134)
(defconstant +chud-c+ 640320)

;; We compute pi/10 instead of pi so the result is in [0,1). Change
;; this constant to 1 if you want True Pi.
;;
;; Note that it is 10 and not 1/10 since the series of Chudnovsky
;; calculates 1/pi.
(defconstant +chud-prefactor+ 10)

(defun make-chudnovsky-series ()
  (flet ((a (n)
           (+ #.(* +chud-a+ (numerator +chud-prefactor+))
              (* n #.(* +chud-b+ (numerator +chud-prefactor+)))))
         (p (n)
           (if (zerop n)
               1
               ;; This is Horner's form of
               ;; -(6n - 5)*(2n - 1)*(6n - 1)
               (+ 5 (* n (+ -46 (* n (+ 108 (* n -72))))))))
         (q (n)
           (if (zerop n)
               1
               (* (expt n 3)
                  #.(/ (expt +chud-c+ 3) 24)))))
    (make-series :a (alexandria:compose #'int #'a)
                 :b (constantly (int #.(* (denominator +chud-prefactor+)
                                          (/ (* 8 +chud-c+) 12))))
                 :p (alexandria:compose #'int #'p)
                 :q (alexandria:compose #'int #'q))))

(defun compute-pi/chudnovsky (prec)
  (let* ((num-terms (floor (+ 2 (/ prec +chud-decimals-per-term+))))
         ;; √640320 = 8√10005
         (sqrt-c    (isqrt (* 10005 (expt 100 prec))))
         (comp      (binary-split (make-chudnovsky-series) 0 num-terms)))
    (values (floor (* sqrt-c (mpz-integer (partial-denominator comp)))
                   (mpz-integer (partial-numerator comp))))))

(defun mpd-pi (prec-bits)
  (let* ((guard-bits (+ prec-bits $digit-bits))
         (num-terms (+ 2 (floor guard-bits +chud-bits-per-term+)))
         ;; intermediate steps:
         comp sqrt recip final)
    (with-stopwatch (tim :log t)
      (format t "~2&terms = ~A~%" num-terms)
      (setf comp (binary-split (make-chudnovsky-series) 0 num-terms))
      (tim "split")

      (setf sqrt (mpd-sqrt (integer-mpd 10005) guard-bits))
      (setf final sqrt)
      (tim "sqrt")

      (setf recip (mpd-reciprocal (mpz-mpd (partial-numerator comp)) guard-bits))
      (setf final (mpd-* final recip))
      (tim "recip")

      (setf final (mpd-* final (mpz-mpd (partial-denominator comp))))
      ;;(mpd-truncate! final :to-bits prec-bits) ; prec, not guard!
      (tim "final"))
    final))
