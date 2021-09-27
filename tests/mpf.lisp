(in-package #:hypergeometrica-tests)

(deftest test-mpf-integer-idempotence ()
  (loop :for i :from -128 :to 128
        :do (is (= i (h::mpf-rational (h::integer-mpf i)))))
  (loop :repeat 10
        :for i := (* (expt -1 (random 2))
                     (random (expt most-positive-fixnum 10)))
        :do (is (= i (h::mpf-rational (h::integer-mpf i))))))

(deftest test-mpf-integer? ()
  (loop :for i :below (* 2 h::$digit-bits)
        :for x := (h::integer-mpf (ash 1 i))
        :do (is (= i (h::mpf-smallest-power x))))
  (loop :repeat 10
        :for i := (* (expt -1 (random 2))
                     (random (expt most-positive-fixnum 10)))
        :do (is (h::mpf-integer? (h::integer-mpf i))))
  (loop :repeat 100
        :for i := (* (expt -1 (random 2))
                     (1+ (random (expt most-positive-fixnum 10))))
        :for x := (h::integer-mpf i)
        :do
           (h::mpf-ash! x (- (1+ (h::count-trailing-zeroes (abs i)))))
           (is (not (h::mpf-integer? x)))))

(deftest test-mpf-test-msbs ()
  (is (not (h::mpf-test-msbs (h::mpf-zero) 3)))
  (is (not (h::mpf-test-msbs (h::integer-mpf #b101) 3)))
  (is (not (h::mpf-test-msbs (h::integer-mpf #b101) (- h::$digit-bits 4))))
  (is (h::mpf-test-msbs (h::integer-mpf #b101) (- h::$digit-bits 3)))
  (is (h::mpf-test-msbs (h::integer-mpf #b101) (- h::$digit-bits 2)))
  (is (h::mpf-test-msbs (h::integer-mpf #b101) (- h::$digit-bits 1)))
  (is (not (h::mpf-test-msbs (h::integer-mpf h::$base) (1- h::$digit-bits))))
  (is (h::mpf-test-msbs (h::integer-mpf h::$base) h::$digit-bits)))

