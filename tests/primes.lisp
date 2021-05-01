;;;; primes.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-garner ()
  (loop :repeat 50000 :do
    (let* ((moduli #(2 3 5 7 11 13 17 19 23 29 31))
           (n (random (reduce #'* moduli)))
           (vals (h::to-congruence-relations moduli n))
           (g (h::garner moduli vals))
           (n* (h::reconstruct-from-garner moduli g)))
      (is (= n n*)))))

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
