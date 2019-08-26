;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun make-ntt-storage (n)
  (make-array n :element-type 'alexandria:non-negative-fixnum
                :initial-element 0))

(defun mpz-* (a b)
  (let* ((size-a (mpz-size a))
         (size-b (mpz-size b))
         (result-size (+ 1 size-a size-b))
         (result-storage (make-storage result-size))
         (length (least-power-of-two->= result-size))
         (a-ntt (make-ntt-storage length))
         (b-ntt (make-ntt-storage length))
         (m (first (find-suitable-moduli (max length (expt $base 2)))))
         (w (ordered-root-from-primitive-root
             (find-primitive-root m)
             length
             m)))
    ;; Fill out the storage.
    (map-into a-ntt #'identity (storage a))
    (map-into b-ntt #'identity (storage b))

    ;; Compute the forward transforms.
    (setf a-ntt (ntt-forward a-ntt :modulus m :primitive-root w))
    (setf b-ntt (ntt-forward b-ntt :modulus m :primitive-root w))

    ;; Pointwise multiply into A-NTT
    (dotimes (i length)
      (setf (aref a-ntt i)
            (m* (aref a-ntt i) (aref b-ntt i) m)))

    ;; Inverse transform
    (setf a-ntt (ntt-reverse a-ntt :modulus m :primitive-root w))

    ;; Unpack the result.
    (loop :with carry := 0
          :for i :below result-size
          :for ci := (+ carry (aref a-ntt i))
          :if (>= ci $base)
            :do (multiple-value-setq (carry ci) (floor ci $base))
          :else
            :do (setf carry 0)
          :do (setf (aref result-storage i) ci)
          :finally (assert (zerop carry))
                   (return (make-instance 'mpz :sign (* (sign a) (sign b))
                                               :storage result-storage)))))

(defun test-* (m n)
  (let* ((a (integer-mpz m))
         (b (integer-mpz n))
         (c (mpz-* a b)))
    (format t "actual: ~D~%" (* m n))
    (format t "ntt   : ~D~%" (mpz-integer c))
    (format t "same: ~A~%" (= (* m n) (mpz-integer c)))
    c))
