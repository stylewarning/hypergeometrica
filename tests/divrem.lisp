;;;; divrem.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-divrem-powers-of-two ()
  (let* ((two (h::integer-mpz 2 'h::mpz/ram))
         (h (h::mpz-expt two 100)))
    (loop :for zeros :from 100 :downto 1 :do
      (setf h (h::mpz-+ h (h::integer-mpz 1 'h::mpz/ram)))
      ;; Check we are going in with the right number.
      (is (= (h::mpz-integer h) (1+ (expt 2 zeros))))
      ;; Divide by two.
      (multiple-value-bind (q r)
          (h::mpz-divrem h two)
        (is (= 1 (h::mpz-integer r)))
        (setf h q)))))

(deftest test-divrem-third ()
  (let* ((three (h::integer-mpz 3 'h::mpz/ram))
         (ten   (h::integer-mpz 10 'h::mpz/ram)))
    (loop :for power :from 1 :to 100000 :by 10000 :do
      (let ((ten (h::mpz-expt ten power)))
        (multiple-value-bind (q r)
            (h::mpz-divrem ten three)
          (multiple-value-bind (qlisp rlisp)
              (floor (expt 10 power) 3)
            (is (= qlisp (h::mpz-integer q)))
            (is (= rlisp (h::mpz-integer r)))))))))
