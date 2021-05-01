;;;; arithmetic.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

(deftest test-arith-intrinsics ()
  "Test the arithmetic intrinsics."
  (multiple-value-bind (sum carry) (h::add64 0 0)
    (is (= sum   0))
    (is (= carry 0)))
  (multiple-value-bind (sum carry) (h::add64 1 1)
    (is (= sum   2))
    (is (= carry 0)))
  (multiple-value-bind (sum carry) (h::add64 (1- (expt 2 64)) 0)
    (is (= sum   (1- (expt 2 64))))
    (is (= carry 0)))
  (multiple-value-bind (sum carry) (h::add64 (1- (expt 2 64)) 1)
    (is (= sum   0))
    (is (= carry 1)))
  (multiple-value-bind (sum carry) (h::add64 (1- (expt 2 64)) 2)
    (is (= sum   1))
    (is (= carry 1)))
  ;; random testing
  (loop :repeat 1000
        :for little-r := (1+ (random 1000))
        :for a64 := (1+ (random (1- (expt 2 64))))
        :for b64 := (1+ (random (1- (expt 2 64))))
        :for r128 := (1+ (random (1- (expt 2 128))))
        :do (let ((ab (* a64 b64)))
              (multiple-value-bind (lo hi) (h::mul128 a64 b64)
                (is (= lo (ldb (byte 64 0) ab)))
                (is (= hi (ldb (byte 64 64) ab))))
              (multiple-value-bind (quo rem) (h::div128 (1+ (ldb (byte 64 0) ab))
                                                        (ldb (byte 64 64) ab)
                                                        a64)
                (is (= quo b64))
                (is (= rem 1)))
              (multiple-value-bind (lo hi) (h::add128 a64 b64 b64 a64)
                (let ((p (+ (+ a64 (ash b64 64))
                            (+ b64 (ash a64 64)))))
                  (is (= lo (ldb (byte 64 0) p)))
                  (is (= hi (ldb (byte 64 64) p)))))
              ;; TODO: test SUB128
              )))

(defun %test-m* (n low)
  (flet ((r (&optional (high h::$base))
           (+ low (random (- high low)))))
    (loop :repeat n
          :for m := (r h::$max-modulus)
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
