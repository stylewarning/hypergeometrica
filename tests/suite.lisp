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

;;;;;;; These are various NTT implementations used for testing ;;;;;;;

(defun ntt-forward-matrix (N m w)
  "Compute the NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N."
  (let ((matrix (make-array (list N N) :initial-element 1)))
    (loop :for row :from 0 :below N :do
      (loop :for col :from 0 :below N
            :for exponent := (* col row)
            :do (setf (aref matrix row col)
                      (h::expt-mod/safe w exponent m)))
          :finally (return matrix))))

(defun ntt-reverse-matrix (N m w)
  "Compute the inverse NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N.

This is just the conjugate-transpose of the NTT matrix, scaled by N."
  (labels ((conjugate-transpose (in)
             (let ((out (make-array (array-dimensions in))))
               (loop :for row :below (array-dimension out 0) :do
                 (loop :for col :below (array-dimension out 1) :do
                   (setf (aref out col row)
                         (h::m/ (h::inv-mod (aref in row col) m) N m))))
               out)))
    (conjugate-transpose (ntt-forward-matrix N m w))))

(defun matmul (A B modulus)
  "Multiply the matrices A and B over Z/mZ for modulus MODULUS."
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (l (array-dimension B 1))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop :for i :below m :do
      (loop :for k :below l :do
        (setf (aref C i k)
              (mod (loop :for j :below n
                         :sum (* (aref A i j)
                                 (aref B j k)))
                   modulus))))
    C))

(defun matvecmul (matrix v m)
  "Multiply the matrix MATRIX by the column vector V over Z/mZ."
  (let* ((N (length v))
         (result (copy-seq v)))
    (loop :for row :below N
          :do (setf (aref result row)
                    (loop :for col :below N
                          :for x := (aref matrix row col)
                          :for y := (aref v col)
                          :for x*y := (h::m* x y m)
                          :for s := x*y :then (h::m+ s x*y m)
                          :finally (return s)))
          :finally (return result))))

(defun ntt-forward-direct (in m w)
  "Compute the NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (h::expt-mod/safe w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (h::expt-mod/safe w^k j m)
                          :sum (h::m* w^jk (aref in j) m) :into s
                          :finally (return (mod s m))))
          :finally (return out))))

(defun ntt-reverse-direct (in m w)
  "Compute the inverse NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (setf w (h::inv-mod w m))
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (h::expt-mod w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (h::expt-mod/safe w^k j m)
                          :sum (* w^jk (aref in j)) :into s
                          :finally (return (h::m/ (mod s m) N m))))
          :finally (return out))))


(defun test-inversion/matrix (v m w)
  "Tests inversion property of matrix method."
  (let* ((N (length v))
         (eye (matmul
               (ntt-reverse-matrix N m w)
               (ntt-forward-matrix N m w)
               m)))
    (is (loop :for i :below N
              :always (loop :for j :below N
                            :always (= (aref eye i j)
                                       (if (= i j)
                                           1
                                           0)))))))

(defun test-inversion/direct (v m w)
  "Tests inversion property of the direct NTTs."
  (is (equalp v
              (ntt-reverse-direct (ntt-forward-direct v m w)
                                  m w))))

(defun test-inversion/ntt (v m w)
  "Tests inversion property of the fast NTTs."
  (is (equalp v
              (h::ntt-reverse (h::ntt-forward (copy-seq v) m w) m w))))


(deftest test-inversion-properties ()
  "Test that the forward and reverse transforms are actually inverses."
  (let ((N (expt 2 6)))
    (dolist (m (append h::*moduli* (h::find-suitable-moduli N :count 15)))
      (let* ((v (make-array N :element-type 'h::ntt-coefficient :initial-element 0))
             (w (h::find-primitive-root N m)))
        (map-into v (lambda () (random m)))
        (test-inversion/matrix v m w)
        (test-inversion/direct v m w)
        (test-inversion/ntt v m w)))))

(deftest test-ntt-from-various-definitions ()
  "Test that the NTTs agree in their transforms."
  (let ((N (expt 2 8)))
    (dolist (m (append h::*moduli* (h::find-suitable-moduli N :count 15)))
      (let* ((v (make-array N :element-type 'h::ntt-coefficient :initial-element 0))
             (w (h::find-primitive-root n m)))
        (map-into v (lambda () (random m)))
        (let ((a (matvecmul (ntt-forward-matrix N m w) v m))
              (b (ntt-forward-direct v m w))
              (c (h::ntt-forward (copy-seq v) m w)))
          (h::bit-reversed-permute! c)
          (is (equalp a b)))
        (let ((a (matvecmul (ntt-reverse-matrix N m w) v m))
              (b (ntt-reverse-direct v m w))
              (c (let ((v (copy-seq v)))
                   (h::bit-reversed-permute! v)
                   (h::ntt-reverse v m w))))
          (is (equalp a b))
          (is (equalp b c)))))))

;;;;;;;;;;;;;;;; Testing Barebones NTT Multiplication ;;;;;;;;;;;;;;;;

(defun digit-count (n)
  "How many decimal digits does it take to write N?"
  (if (zerop n)
      1
      (let* ((approx   (ceiling (integer-length n) (log 10.0d0 2)))
             (exponent (expt 10 (1- approx))))
        (if (> exponent n)
            (1- approx)
            approx))))

(defun digits (n &key (size (digit-count n)))
  "Make an array of the decimal digits of N."
  (loop :with v := (make-array size :initial-element 0 :element-type 'h::digit)
        :for i :from 0
        :while (plusp n) :do
          (multiple-value-bind (div rem) (floor n 10)
            (setf (aref v i) rem)
            (setf n div))
        :finally (return v)))

(defun carry (v)
  "Perform carry propagation on the vector V."
  (loop :for i :below (1- (length v))
        :when (<= 10 (aref v i))
          :do (multiple-value-bind (div rem) (floor (aref v i) 10)
                (setf (aref v i)      rem)
                (incf (aref v (1+ i)) div))
        :finally (return v)))

(defun undigits (v)
  "Take an array of decimal digits V and create a number N."
  (loop :for x :across (carry v)
        :for b := 1 :then (* 10 b)
        :sum (* b x)))

;;; A convolution whose length-N input sequences have a maximum value
;;; of M will have values bounded by N*M^2. Our prime must be larger
;;; than this.
(defun simplistic-ntt-multiply (a b)
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (h::least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length))
         (m (first (h::find-suitable-moduli (* length (expt 10 2)))))
         (w (h::find-primitive-root length m)))
    (when h::*verbose*
      (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

      (format t "A's digits: ~A~%" a-digits)
      (format t "B's digits: ~A~%" b-digits))

    (setf a-digits (h::ntt-forward a-digits m w))
    (setf b-digits (h::ntt-forward b-digits m w))

    (when h::*verbose*
      (format t "NTT(A): ~A~%" a-digits)
      (format t "NTT(B): ~A~%" b-digits))

    (setf a-digits (map-into a-digits (lambda (a b) (h::m* a b m)) a-digits b-digits))

    (when h::*verbose*
      (format t "C = NTT(A)*NTT(B) mod ~D = ~A~%" m a-digits))

    (setf a-digits (h::ntt-reverse a-digits m w))

    (when h::*verbose*
      (format t "NTT^-1(C): ~A~%" a-digits))

    (undigits a-digits)))

(defun simplistic-fft-multiply (a b)
  "Multiply two non-negative integers A and B using FFTs."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (h::least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (coerce (digits a :size length) 'simple-vector))
         (b-digits (coerce (digits b :size length) 'simple-vector)))
    (when h::*verbose*
      (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

      (format t "A's digits: ~A~%" a-digits)
      (format t "B's digits: ~A~%" b-digits))

    (setf a-digits (h::dif-forward a-digits))
    (setf b-digits (h::dif-forward b-digits))

    (when h::*verbose*
      (format t "FFT(A): ~A~%" a-digits)
      (format t "FFT(B): ~A~%" b-digits))

    (setf a-digits (map-into a-digits #'* a-digits b-digits))

    (when h::*verbose*
      (format t "C = FFT(A)*FFT(B) = ~A~%" a-digits))

    (setf a-digits (h::dif-reverse a-digits))

    (when h::*verbose*
      (format t "FFT^-1(C): ~A~%" a-digits))

    (undigits (map 'vector (lambda (z)
                             (round (realpart z)))
                   a-digits))))

(deftest test-simplistic-ntt-multiply ()
  (loop :repeat 1000
        :for a := (random most-positive-fixnum)
        :for b := (random most-positive-fixnum)
        :do (is (= (* a b) (simplistic-ntt-multiply a b)))))

(deftest test-simplistic-fft-multiply ()
  (loop :repeat 1000
        :for a := (random most-positive-fixnum)
        :for b := (random most-positive-fixnum)
        :do (is (= (* a b) (simplistic-fft-multiply a b)))))

