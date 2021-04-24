;;;; multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; Multiplication Driver

(defparameter *ntt-multiply-threshold* 100
  "Up to how many digits can the smaller number of a multiplication have before NTT multiplication is used?")

(defun mpz-* (x y)
  (optimize-mpz x)
  (optimize-mpz y)
  (when (< (mpz-size x) (mpz-size y))
    (rotatef x y))

  ;; Now the size of X is guaranteed greater-or-equal Y.
  (optimize-mpz
   (cond
     ((mpz-zerop y)
      (integer-mpz 0))
     ((= 1 (mpz-size y))
      (let ((d (vec-ref (storage y) 0)))
        (cond
          ((= 1 d)
           (if (= -1 (sign y))
               (mpz-negate x)
               x))
          (t
           (let ((r (make-mpz (* (sign x) (sign y))
                              (make-storage (+ 2 (mpz-size x))))))
             (vec-replace/unsafe (storage r) (storage x))
             (mpz-multiply-by-digit! d r)
             r)))))
     ((<= (mpz-size y) *ntt-multiply-threshold*)
      (make-mpz (* (sign x) (sign y))
                (%multiply-storage/schoolboy
                 (storage x) (mpz-size x)
                 (storage y) (mpz-size y))))
     ((eq x y)
      (mpz-square x))
     #+hypergeometrica-floating-point
     ((< (least-power-of-two->= (+ 1 (mpz-size x) (mpz-size y))) +fft-length-limit+)
      (mpz-*/fft x y))
     (t
      (mpz-*/ntt x y)))))

(defun f-expt (a n one multiply)
  "Exponentiate A (any object) to the power of N (a non-negative integer), where ONE is multiplicative identity and MULTIPLY is the binary multiplication function."
  (check-type n (integer 0))
  (cond
    ((zerop n) one)
    ((= 1 n)   a)
    (t
     (let ((k (integer-length n))
           (x a))
       (loop :for bit :from (- k 2) :downto 0
             :do (progn
                   (when *verbose*
                     (let* ((current-exponent (ldb (byte (- k bit) bit) n))
                            (total-exponent   n)
                            (percentage-complete (* 100.0d0
                                                    (/ current-exponent total-exponent))))
                       (format t "~&-------------------------~%")
                       (format t "~&Current exponent: ~D / ~D (~6,2F%)~%"
                               current-exponent
                               total-exponent
                               percentage-complete)))
                   (setf x (funcall multiply x x))
                   (when  (logbitp bit n)
                     (setf x (funcall multiply x a))))
             :finally (return x))))))

(defun mpz-expt (a n)
  "Raise an MPZ A to the power of a non-negative integer N."
  (f-expt a n (integer-mpz 1) #'mpz-*))
