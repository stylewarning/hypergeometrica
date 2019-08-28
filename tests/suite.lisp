;;;; suite.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(defun test-hypergeometrica ()
  (let ((h::*verbose* nil))
    (run-package-tests :package ':hypergeometrica-tests
                       :verbose nil
                       :describe-failures t
                       :interactive t)))

;;; Tests start HERE!

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
        :do (is (= i (h::next-power-of-two j)))))


(defun %test-m* (n low)
  (flet ((r (&optional (high h::$base))
           (+ low (random (- high low)))))
    (loop :repeat n
          :for m := (r)
          :for a := (r m)
          :for b := (r m)
          :for x := (h::m* a b m)
          :for y := (mod (* a b) m)
          :do (is (= x y)))))

(deftest test-m* ()
  ;; TODO test all mod functions
  (%test-m* 10000 0)
  (%test-m* 10000 1000)
  (%test-m* 10000 1000000)
  (%test-m* 10000 1000000000)
  (%test-m* 10000 1000000000000))

(deftest test-factor-out ()
  (flet ((test-it (p k)
           (multiple-value-bind (n pow) (h::factor-out (* (expt p k) 7) p)
             (is (and (= 7 n)
                      (= k pow))))))
    (test-it 2 1)
    (test-it 2 2)
    (test-it 2 3)
    (test-it 3 1)
    (test-it 3 2)
    (test-it 3 3)
    (test-it 6 1)
    (test-it 6 2)
    (test-it 6 3)))

(deftest test-primep ()
  (let ((primes-from-the-internet
          '(2      3      5      7     11     13     17     19     23     29
            31     37     41     43     47     53     59     61     67     71
            73     79     83     89     97    101    103    107    109    113
            127    131    137    139    149    151    157    163    167    173
            179    181    191    193    197    199    211    223    227    229
            233    239    241    251    257    263    269    271    277    281
            283    293    307    311    313    317    331    337    347    349
            353    359    367    373    379    383    389    397    401    409
            419    421    431    433    439    443    449    457    461    463
            467    479    487    491    499    503    509    521    523    541
            547    557    563    569    571    577    587    593    599    601
            607    613    617    619    631    641    643    647    653    659
            661    673    677    683    691    701    709    719    727    733
            739    743    751    757    761    769    773    787    797    809
            811    821    823    827    829    839    853    857    859    863
            877    881    883    887    907    911    919    929    937    941
            947    953    967    971    977    983    991    997)))
    (is (every #'h::primep primes-from-the-internet))
    (loop :for p :in primes-from-the-internet
          :for p-next :in (rest primes-from-the-internet)
          :do (is (= p-next (h::next-prime p))))))

(deftest test-factorize ()
  (loop :repeat 100
        :for n := (1+ (random 100000))
        :for factorization := (h::factorize n)
        :do (loop :with x := 1
                  :for (f . k) :in factorization
                  :do (setf x (* x (expt f k)))
                  :finally (is (= x n)))))

(deftest test-finding-moduli ()
  (loop :for k :from 2 :to 55
        :for moduli := (h::find-suitable-moduli (expt 2 k) :count 5)
        :do (is (every #'h::primep moduli))
            (is (every (lambda (m)
                         (<= k (nth-value 1 (h::factor-out (1- m) 2))))
                       moduli))))

(deftest test-primitive-root ()
  (let* ((moduli (h::find-suitable-moduli (expt 2 9) :count 100))
         (roots  (mapcar #'h::find-primitive-root moduli)))
    (is (every #'h::naive-primitive-root-p roots moduli))))


(deftest test-mpz-integer-idempotence ()
  (dotimes (i 100)
    (is (= i (h::mpz-integer (h::integer-mpz i)))))
  (loop :repeat 10
        :for n := (* (expt -1 (random 2))
                     (expt (random most-positive-fixnum) (random 10000)))
        :do (is (= n (h::mpz-integer (h::integer-mpz n))))))

(deftest test-plus-minus-zero ()
  (is (h::mpz-plusp (h::integer-mpz 1)))
  (is (h::mpz-zerop (h::integer-mpz 0)))
  (is (h::mpz-minusp (h::integer-mpz -1))))

(deftest test-mpz-plus-minus-times ()
  (flet ((r ()
           (- (floor (expt 10 10000) 2) (random (* 2 (expt 10 10000))))))
    (loop :repeat 10
          :for a := (r)
          :for za := (h::integer-mpz a)
          :for b := (r)
          :for zb := (h::integer-mpz b)
          :do (is (= (+ a b) (h::mpz-integer (h::mpz-+ za zb))))
              (is (= (- a b) (h::mpz-integer (h::mpz-- za zb))))
              (is (= (* a a) (h::mpz-integer (h::mpz-square za))))
              (is (= (* a b) (h::mpz-integer (h::mpz-* za zb)))))))
