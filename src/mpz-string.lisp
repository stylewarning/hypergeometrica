;;;; mpz-string.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

;;; ALGORITHM Out
;;;
;;; INPUT: integer A of N digits
;;;
;;; OUTPUT: string S in base B
;;;
;;; if A < B
;;;     return char(A)
;;; else
;;;     find k such that B^(2k-2) <= A < B^(2k)
;;;     Q, R = divrem(A, B^k)
;;;     r = Out(R)
;;;     return Out(Q) ++ ["0" * (k - len(r))] ++ r



(defgeneric write-number (x stream))

(defmethod write-number ((x integer) stream)
  (format stream "~D" x))

(defmethod write-number ((x mpz/ram) stream)
  (cond
    ((mpz-minusp x)
     (write-string "-" stream)
     (write-number (mpz-abs x) stream)
     nil)
    ((mpz-zerop x)
     (write-string "0" stream)
     nil)
    ((= 1 (mpz-size x))
     (write-number (mpz-digit x 0) stream)
     nil)
    (t
     (%write-big-number x stream)
     nil)))

(defun %write-big-number (x stream)
  ;;(format t "~& x = ~D~%" (mpz-integer x))
  (cond
    ((and (= 1 (mpz-size x))
          (< (mpz-digit x 0) $largest-power-of-10))
     (write-number (mpz-digit x 0) stream))
    (t
     (let* ((l (biggest-power-of-10<= x))
            (k (floor (+ l 2) 2)))
       (multiple-value-bind (quo rem)
           (mpz-divrem x (mpz-expt (integer-mpz 10 'mpz/ram) k))
         ;;(format t "quo = ~D~%" (mpz-integer quo))
         ;;(format t "rem = ~D~%" (mpz-integer rem))
         (let ((r (with-output-to-string (s)
                    (%write-big-number rem s))))
           (%write-big-number quo stream)
           (loop :repeat (- k (length r)) :do (write-char #\0 stream))
           (write-string r stream)))))))

(defun biggest-power-of-10<= (x)
  (let* ((bits (mpz-bit-size x))
         ;; This division is possibly an *OVER* estimate.
         (est (floor (/ (max 0 (1- bits)) (log 10.0d0 2.0d0))))
         (10^n (mpz-expt (integer-mpz 10 'mpz/ram) est)))
    #+hypergeometrica-paranoid
    (assert (mpz-<= 10^n x)
        ()
        "expected ~D <= ~D"
        (mpz-integer 10^n)
        (mpz-integer x))

    (loop :do
      (mpz-multiply-by-digit! 10 10^n)
      (incf est)
      (when (mpz-> 10^n x)
        (decf est)
        (loop-finish)))
    
    est))

(defun largest10 (n)
  (loop :with i := 0
        :while (plusp n)
        :do (setf n (floor n 10))
            (incf i)
        :finally (return (1- i))))

(defun largest10* (bits)
  (floor (/ bits (log 10.0d0 2.0d0))))

(defun pow-2-10 (n)
  (loop :for i :below n :do
    (format t "2^~A > 10^~A = ~A = ~A?~%"
            i
            (largest10 (expt 2 i))
            (largest10* i)
            (biggest-power-of-10<= (integer-mpz (expt 2 i) 'mpz/ram))
            ))
  )
