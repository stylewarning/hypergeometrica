;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun make-ntt-storage (n)
  (make-array n :element-type 'alexandria:non-negative-fixnum
                :initial-element 0))

(defun mpz-* (&rest xs)
  (declare (dynamic-extent xs))
  (cond
    ((null xs)       (integer-mpz 1))
    ((null (cdr xs)) (car xs))
    ;; We are guaranteed at least two elements here.
    (t
     (let* ((num-factors (length xs))
            (result-size (+ (1- num-factors)
                            (reduce #'+ xs :key #'mpz-size)))
            (result-storage (make-storage result-size))
            (length (least-power-of-two->= result-size))
            (result-ntt (make-ntt-storage length))
            (temp-ntt (make-ntt-storage length))
            ;; XXX: Can we avoid having to do BASE^N?
            (m (first (find-suitable-moduli (max length (expt $base num-factors)))))
            (w (ordered-root-from-primitive-root
                (find-primitive-root m)
                length
                m)))
       ;; Copy one of the factors and transform it
       (replace result-ntt (storage (car xs)))
       (setf result-ntt (ntt-forward result-ntt :modulus m :primitive-root w))

       ;; Multiply in the other factors one at a time.
       (dolist (x (cdr xs))
         ;; Clear
         (fill temp-ntt 0)
         ;; Copy
         (replace temp-ntt (storage x))
         ;; Transform
         (setf temp-ntt (ntt-forward temp-ntt :modulus m :primitive-root w))
         ;; Pointwise multiply
         (dotimes (i length)
           (setf (aref result-ntt i)
                 (m* (aref result-ntt i) (aref temp-ntt i) m))))

       ;; Inverse transform
       (setf result-ntt (ntt-reverse result-ntt :modulus m :primitive-root w))

       ;; Unpack the result.
       (loop :with carry := 0
             :for i :below result-size
             :for ci := (+ carry (aref result-ntt i))
             :if (>= ci $base)
               :do (multiple-value-setq (carry ci) (floor ci $base))
             :else
               :do (setf carry 0)
             :do (setf (aref result-storage i) ci)
             :finally (assert (zerop carry))
                      (return (make-instance 'mpz :sign (reduce #'* xs :key #'sign)
                                                  :storage result-storage)))))))

(defun test-* (&rest xs)
  (let* ((p (apply #'mpz-* (mapcar #'integer-mpz xs)))
         (q (apply #'* xs)))
    (format t "actual: ~D~%" q)
    (format t "ntt   : ~D~%" (mpz-integer p))
    (format t "same: ~A~%" (= (mpz-integer p) q))
    p))
