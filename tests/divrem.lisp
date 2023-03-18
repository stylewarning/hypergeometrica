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
