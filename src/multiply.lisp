;;;; multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; Multiplication Driver

(defparameter *ntt-multiply-threshold* 100
  "Up to how many digits can the smaller number of a multiplication have before NTT multiplication is used?")

(defun mpz-* (x y)
  (optimize-storage x)
  (optimize-storage y)
  (when (< (mpz-size x) (mpz-size y))
    (rotatef x y))
  ;; Now the size of X is guaranteed greater-or-equal Y.
  (optimize-storage
   (cond
     ((mpz-zerop y)
      (integer-mpz 0))
     ((= 1 (mpz-size y))
      (let ((d (aref (storage y) 0)))
        (cond
          ((= 1 d)
           (if (= -1 (sign y))
               (mpz-negate x)
               x))
          (t
           (let ((r (make-mpz (* (sign x) (sign y))
                              (make-storage (+ 2 (mpz-size x))))))
             (replace (raw-storage r) (raw-storage x))
             (mpz-multiply-by-digit! d r)
             r)))))
     ((<= (mpz-size y) *ntt-multiply-threshold*)
      (let ((r-storage (%multiply-storage/schoolboy
                        (raw-storage x) (mpz-size x)
                        (raw-storage y) (mpz-size y))))
        (make-mpz (* (sign x) (sign y)) r-storage)))
     ((eq x y)
      (mpz-square x))
     #+hypergeometrica-floating-point
     ((< (least-power-of-two->= (+ 1 (mpz-size x) (mpz-size y))) +fft-length-limit+)
      (mpz-*/fft x y))
     (t
      (mpz-*/ntt x y)))))
