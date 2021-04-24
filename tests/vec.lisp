;;;; vec.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-create-ram-vec ()
  (loop :for length :below 8192
        :for length/2 := (floor length 2)
        :for vec := (h::make-ram-vec length)
        :do (is (= length (h::vec-digit-length vec)))
            (h::resize-vec-by vec length/2)
            (is (= (+ length length/2) (h::vec-digit-length vec)))
            (h::resize-vec-by vec (- length/2))
            (is (= length (h::vec-digit-length vec)))
            (h::resize-vec-by vec (- length/2))
            (is (= (- length length/2) (h::vec-digit-length vec)))))
