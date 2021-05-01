;;;; moduli.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-scheme-is-sufficient ()
  (is (<= 3 (length (h::scheme-moduli h::**scheme**))))
  (is (<= 50 (h::scheme-max-transform-length h::**scheme**)))
  (is (<= (+ 64 64 50) (reduce #'+ (h::scheme-moduli h::**scheme**) :key #'h::lg))))

(deftest test-finding-moduli ()
  (loop :for k :from 2 :to 55
        :for moduli := (h::find-suitable-moduli (expt 2 k) :count 5)
        :do (is (every #'h::primep moduli))
            (is (every (lambda (m)
                         (<= k (nth-value 1 (h::factor-out (1- m) 2))))
                       moduli))))

(deftest test-primitive-root ()
  (let* ((N (expt 2 5))
         (moduli (h::find-suitable-moduli N :count 100))
         (generators  (mapcar #'h::find-finite-field-generator moduli))
         (roots (mapcar (lambda (g m) (h::primitive-root-from-generator g N m))
                        generators
                        moduli)))
    (is (every #'h::naive-generator-p generators moduli))
    (flet ((primitive-nth-root-p (w m)
             (h::naive-primitive-root-p w N m)))
      (is (every #'primitive-nth-root-p roots moduli)))))
