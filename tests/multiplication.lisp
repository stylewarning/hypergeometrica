;;;; multiplication.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

;;;;;;;;;;;;;;;; Testing fast multiplication ;;;;;;;;;;;;;;;;

(deftest test-m*/fast ()
  (let ((moduli (h::scheme-moduli h::**scheme**)))
    (dotimes (nmod (length moduli))
      (let* ((m (aref moduli nmod))
             (mi (aref (h::scheme-inverses h::**scheme**) nmod)))
        (loop :repeat 100000 :do
                (let ((a (random m))
                      (b (random m)))
                  (is (= (mod (* a b) m)
                         (h::m*/fast a b m mi)))))))))

;;;;;;;;;;;;;;;; Testing Barebones NTT Multiplication ;;;;;;;;;;;;;;;;

(defun digit-count (n)
  "How many decimal digits does it take to write N?"
  (if (zerop n)
      1
      (let* ((approx   (ceiling (integer-length n) (log 10.0d0 2)))
             (exponent (expt 10 (1- approx))))
        (if (> exponent n)
            (1- approx)
            approx))))

(defun digits (n &key (size (digit-count n)))
  "Make an array of the decimal digits of N."
  (loop :with v := (make-array size :initial-element 0 :element-type 'h::digit)
        :for i :from 0
        :while (plusp n) :do
          (multiple-value-bind (div rem) (floor n 10)
            (setf (aref v i) rem)
            (setf n div))
        :finally (return v)))

(defun carry (v)
  "Perform carry propagation on the vector V."
  (loop :for i :below (1- (length v))
        :when (<= 10 (aref v i))
          :do (multiple-value-bind (div rem) (floor (aref v i) 10)
                (setf (aref v i)      rem)
                (incf (aref v (1+ i)) div))
        :finally (return v)))

(defun undigits (v)
  "Take an array of decimal digits V and create a number N."
  (loop :for x :across (carry v)
        :for b := 1 :then (* 10 b)
        :sum (* b x)))

;;; A convolution whose length-N input sequences have a maximum value
;;; of M will have values bounded by N*M^2. Our prime must be larger
;;; than this.
(defun simplistic-ntt-multiply (a b)
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (h::least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length))
         (m (aref (h::scheme-moduli h::**scheme**) 0))
         (scheme (h::make-modular-scheme (list m))))
    (when h::*verbose*
      (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

      (format t "A's digits: ~A~%" a-digits)
      (format t "B's digits: ~A~%" b-digits))

    (setf a-digits (h::ntt-forward a-digits scheme 0))
    (setf b-digits (h::ntt-forward b-digits scheme 0))

    (when h::*verbose*
      (format t "NTT(A): ~A~%" a-digits)
      (format t "NTT(B): ~A~%" b-digits))

    (setf a-digits (map-into a-digits (lambda (a b) (h::m* a b m)) a-digits b-digits))

    (when h::*verbose*
      (format t "C = NTT(A)*NTT(B) mod ~D = ~A~%" m a-digits))

    (setf a-digits (h::ntt-reverse a-digits scheme 0))

    (when h::*verbose*
      (format t "NTT^-1(C): ~A~%" a-digits))

    (undigits a-digits)))

(defun simplistic-fft-multiply (a b)
  "Multiply two non-negative integers A and B using FFTs."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (h::least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (coerce (digits a :size length) 'simple-vector))
         (b-digits (coerce (digits b :size length) 'simple-vector)))
    (when h::*verbose*
      (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

      (format t "A's digits: ~A~%" a-digits)
      (format t "B's digits: ~A~%" b-digits))

    (setf a-digits (h::dif-forward a-digits))
    (setf b-digits (h::dif-forward b-digits))

    (when h::*verbose*
      (format t "FFT(A): ~A~%" a-digits)
      (format t "FFT(B): ~A~%" b-digits))

    (setf a-digits (map-into a-digits #'* a-digits b-digits))

    (when h::*verbose*
      (format t "C = FFT(A)*FFT(B) = ~A~%" a-digits))

    (setf a-digits (h::dit-reverse a-digits))

    (when h::*verbose*
      (format t "FFT^-1(C): ~A~%" a-digits))

    (undigits (map 'vector (lambda (z)
                             (round (realpart z)))
                   a-digits))))

(deftest test-simplistic-ntt-multiply ()
  (loop :repeat 1000
        :for a := (random most-positive-fixnum)
        :for b := (random most-positive-fixnum)
        :do (is (= (* a b) (simplistic-ntt-multiply a b)))))

(deftest test-simplistic-fft-multiply ()
  (loop :repeat 1000
        :for a := (random most-positive-fixnum)
        :for b := (random most-positive-fixnum)
        :do (is (= (* a b) (simplistic-fft-multiply a b)))))

