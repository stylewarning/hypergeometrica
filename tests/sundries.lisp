;;;; sundries.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

;;;; Tests which don't fit anywhere else ;;;;

(deftest test-sundries ()
  ;; POWER-OF-TWO-P
  (is (not (h::power-of-two-p 0)))
  (is (h::power-of-two-p 1))
  (is (h::power-of-two-p 2))
  (is (not (h::power-of-two-p 3)))
  (is (h::power-of-two-p 4))
  (loop :for i :from 1 :to 25
        :do (is (h::power-of-two-p (expt 2 i)))
            (is (not (h::power-of-two-p (+ 3 (expt 2 i))))))
  ;; NEXT-POWER-OF-TWO
  (loop :for i :from 1 :to 25
        :for j := (expt 2 i)
        :do (is (= i (h::next-power-of-two j))))

  (loop :for i :from 2 :to 25
        :for j := (+ 3 (expt 2 i))
        :do (is (= (+ 1 i) (h::next-power-of-two j))))

  (is (h::coprimep 2 3))
  (is (h::coprimep 4 9))
  (is (not (h::coprimep 4 20)))

  (is (h::pairwise-coprimep '(2 3 5 7 11)))
  (is (h::pairwise-coprimep #(2 3 5 7 11)))

  (is (not (h::pairwise-coprimep '(2 3 5 7 9 11))))
  (is (not (h::pairwise-coprimep #(2 3 5 7 9 11)))))
