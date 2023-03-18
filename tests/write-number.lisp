;;;; write-number.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-write-number-randomly ()
  (loop :repeat 150
        :for base := (+ 2 (random 98))
        :for expt := (+ 100 (random 100))
        :for hype-result := (h::mpz-expt (h::integer-mpz base 'h::mpz/ram) expt)
        :for lisp-result := (expt base expt)
        :do
           (is (string=
                (prin1-to-string lisp-result)
                (with-output-to-string (s)
                  (h::write-number hype-result s))))))
