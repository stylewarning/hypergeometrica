;;;; mpz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-mpz-size ()
  (is (zerop (h::mpz-size (h::integer-mpz 0 'h::mpz/ram))))
  (is (= 1 (h::mpz-size (h::integer-mpz 1 'h::mpz/ram))))
  (is (= 2 (h::mpz-size (h::integer-mpz h::$base 'h::mpz/ram)))))

(deftest test-mpz-integer-idempotence ()
  (dotimes (i 100)
    (is (= i (h::mpz-integer (h::integer-mpz i 'h::mpz/ram)))))
  (loop :repeat 10
        :for n := (* (expt -1 (random 2))
                     (expt (random most-positive-fixnum) (random 10000)))
        :do (is (= n (h::mpz-integer (h::integer-mpz n 'h::mpz/ram))))))

(deftest test-plus-minus-zero ()
  (is (h::mpz-plusp (h::integer-mpz 1 'h::mpz/ram)))
  (is (h::mpz-zerop (h::integer-mpz 0 'h::mpz/ram)))
  (is (h::mpz-minusp (h::integer-mpz -1 'h::mpz/ram))))

(deftest test-mpz-mult-simple ()
  (let* ((k (expt 2 128))
         (n (h::integer-mpz k 'h::mpz/ram)))
    (is (= k (h::mpz-integer n)))       ; Sanity check
    (is (= (h::mpz-integer (h::mpz-* n n))
           (* k k)))))

(deftest test-mpz-plus-minus-times-randomly ()
  (flet ((r ()
           (- (floor (expt 10 100) 2) (random (* 2 (expt 10 100))))))
    (loop :repeat 10
          :for a := (r)
          :for za := (h::integer-mpz a 'h::mpz/ram)
          :for b := (r)
          :for zb := (h::integer-mpz b 'h::mpz/ram)
          :do (is (= (+ a b) (h::mpz-integer (h::mpz-+ za zb))))
              (is (= (- a b) (h::mpz-integer (h::mpz-- za zb))))
              (is (= (* a a) (h::mpz-integer (h::mpz-square za))))
              (let ((ab (* a b)))
                (let ((h::*ntt-multiply-threshold* most-positive-fixnum))
                  (is (= ab (h::mpz-integer (h::mpz-* za zb)))))
                (let ((h::*ntt-multiply-threshold* 0))
                  (is (= ab (h::mpz-integer (h::mpz-* za zb)))))))))

(deftest test-s64*mpz ()
  (flet ((test (a b)
           (= (* a b)
              (let ((x (h::integer-mpz b 'h::mpz/ram)))
                (h::mpz-multiply-by-s64! a x)
                (h::mpz-integer x)))))
    (is (test 0 0))
    (is (test 1 1))
    (is (test 1 most-positive-fixnum))
    (is (test -2 2))
    (is (test 2 -2))
    (is (test most-positive-fixnum 1))
    (is (test most-positive-fixnum most-positive-fixnum))
    (is (test 1 (expt most-positive-fixnum 3)))
    (is (test 2 (expt most-positive-fixnum 3)))
    (is (test most-positive-fixnum (expt most-positive-fixnum 3)))
    (is (test (- (random most-positive-fixnum) (floor most-positive-fixnum 2))
              (expt (+ most-positive-fixnum (random 31337)) (+ 2 (random 10)))))))
