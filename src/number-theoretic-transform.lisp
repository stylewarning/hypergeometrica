;;;; number-theoretic-transform.lisp
;;;;
;;;; Copyright (c) 2014-2019 Robert Smith

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:hypergeometrica)

;;;;;;;;;;;;;;;;;;;;;;; Fixed-Width Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline split-byte))
(defun split-byte (x bits)
  "Split the non-negative integer X into two values X0 and X1 such that

    X = X1 << bits + X0."
  (values (ldb (byte bits 0) x)
          (ash x (- bits))))

(declaim (inline join-bytes))
(defun join-bytes (x0 x1 bits)
  "Join the bytes X0 and X1 into a value X such that

    X = X1 << bits + X0.

Ideally BITS is greater than the size of X0."
  (+ x0 (ash x1 bits)))

(defun fixed-width-add (a b width &aux (width/2 (ash width -1)))
  "Add the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not exceed WIDTH bits.

Return two values S0 and S1 such that

    A+B = S0 + S1 << WIDTH."
  (multiple-value-bind (a0 a1) (split-byte a width/2)
    (multiple-value-bind (b0 b1) (split-byte b width/2)
      (multiple-value-bind (low carry) (split-byte (+ a0 b0) width/2)
        (multiple-value-bind (high carry) (split-byte (+ carry a1 b1) width/2)
          (values (join-bytes low high width/2) carry))))))

(defun fixed-width-multiply (a b width &aux (width/2 (ash width -1)))
  "Multiply the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not wxceed WIDTH bits.

Return two values P0 and P1 such that

    A*B = P0 + P1 << WIDTH."
  ;; Split operands into half-width components.
  (multiple-value-bind (a0 a1) (split-byte a width/2)
    (multiple-value-bind (b0 b1) (split-byte b width/2)
      ;; Compute partial products. If W = 2^WIDTH and W' = W/2, then
      ;;
      ;;   A   = A0 + A1*W'
      ;;   B   = B0 + B1*W'
      ;;
      ;;   A*B = (A0 + A1*W')*(B0 + B1*W')
      ;;       = A0*B0 + (A0*B1 + A1*B0)*W' + A1*B1*W
      ;;
      ;; Each of these sub-A*B products are of width WIDTH, and are
      ;; broken into half-width components as above, except for the
      ;; product C3 = A1*B1.
      (multiple-value-bind (c0-lo c0-hi) (split-byte (* a0 b0) width/2)
        (multiple-value-bind (c1a-lo c1a-hi) (split-byte (* a0 b1) width/2)
          (multiple-value-bind (c1b-lo c1b-hi) (split-byte (* a1 b0) width/2)
            (let ((c3 (* a1 b1)))
              ;; Compute columns and carries as in longhand
              ;; multiplication. Each column tracks WIDTH/2 bits.
              ;;
              ;; Column 0   = C0-LO
              ;; Column 1   = C0-HI + C1A-LO + C1B-LO
              ;; Column 2,3 = C1A-HI + C1B-HI + C3 + COL1-CARRY
              (multiple-value-bind (col11 col1-carry1) (fixed-width-add c1a-lo c1b-lo width/2)
                (multiple-value-bind (col1 col1-carry2) (fixed-width-add col11 c0-hi width/2)
                  (let ((col1-carry (+ col1-carry1 col1-carry2)))
                    (values (join-bytes c0-lo col1 width/2)
                            (+ c1a-hi
                               c1b-hi
                               c3
                               col1-carry))))))))))))


;;;;;;;;;;;;;;;;;;;;; Number-Theoretic Transform ;;;;;;;;;;;;;;;;;;;;;

;;; Decimation-in-frequency algorithm.
(defun ntt-forward (a &key ((:modulus m) (first (find-suitable-moduli (length a))))
                           ((:primitive-root w) (find-primitive-root (length a) m)))
  "Compute the forward number-theoretic transform of the array of integers A, with modulus MODULUS and primitive root PRIMITIVE-ROOT. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length.

The resulting array (a mutation of the input) will be in bit-reversed order."
  ;;(format t "m=#x~X (~D)    w=~D~%" m m w)
  (let* ((N  (length a))
         (ln (1- (integer-length N))))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m* w^j (m- u v m) m))))
          (setf w^j (m* w w^j m)))
        (setf w (m* w w m))))

    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m+ (aref a r) (aref a (1+ r)) m)
             (aref a (1+ r)) (m- (aref a r) (aref a (1+ r)) m))))

  a)

;;; Decimation-in-time algorithm.
(defun ntt-reverse (a &key ((:modulus m) (first (find-suitable-moduli (length a))))
                           ((:primitive-root w) (find-primitive-root (length a) m)))
  "Compute the inverse number-theoretic transform of the array of integers A, with modulus MODULUS and primitive root PRIMITIVE-ROOT. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length.

The input must be in bit-reversed order."
  ;;(format t "m=#x~X (~D)    w=~D~%" m m w)
  (setf w (inv-mod w m))
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m/ (m+ (aref a r) (aref a (1+ r)) m) N m)
             (aref a (1+ r)) (m/ (m- (aref a r) (aref a (1+ r)) m) N m)))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((subn (ash 1 ldm))
             (subn/2 (floor subn 2))
             (dw (expt-mod w (ash 1 (- ldn ldm)) m))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (m* w^j (aref a r+j+subn/2) m)))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m- u v m))))
          (setf w^j (m* dw w^j m))))))

  a)

#+#:ignore-DIF
(defun ntt-reverse (a &key ((:modulus m) (first (find-suitable-moduli (length a))))
                           ((:primitive-root w) (ordered-root-from-primitive-root
                                                 (find-primitive-root m)
                                                 (length a)
                                                 m)))
  "Compute the inverse number-theoretic transform of the array of integers A, with modulus MODULUS and primitive root PRIMITIVE-ROOT. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length."
  (format t "m=#x~X (~D)    w=~D~%" m m w)
  (setf w (inv-mod w m))
  (let* ((N   (length a))
         (ln (1- (integer-length N))))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m* w^j (m- u v m) m))))
          (setf w^j (m* w w^j m)))
        (setf w (m* w w m))))

    ;; This includes normalization.
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m/ (m+ (aref a r) (aref a (1+ r)) m) N m)
             (aref a (1+ r)) (m/ (m- (aref a r) (aref a (1+ r)) m) N m))))

  (bit-reversed-permute! a)

  a)


;;;;;;;;;;;;;;;;;;;; Reference DIF FFT algorithm ;;;;;;;;;;;;;;;;;;;;;

(defun dif-forward (a)
  "Compute the radix-2 decimation-in-frequency FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (loop :for ldm :from ldn :downto 2 :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* 2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (aref a r+j+m/2)))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (* w^j (- u v))))))))

    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (+ (aref a r) (aref a (1+ r)))
             (aref a (1+ r)) (- (aref a r) (aref a (1+ r))))))

  (bit-reversed-permute! a)

  a)

(defun dif-reverse (a)
  "Compute the radix-2 decimation-in-frequency inverse FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (loop :for ldm :from ldn :downto 2 :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* -2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (aref a r+j+m/2)))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (* w^j (- u v))))))))

    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (/ (+ (aref a r) (aref a (1+ r))) N)
             (aref a (1+ r)) (/ (- (aref a r) (aref a (1+ r))) N))))

  (bit-reversed-permute! a)

  a)

(defun dit-forward (a)
  "Compute the radix-2 decimation-in-time FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (bit-reversed-permute! a)
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (+ (aref a r) (aref a (1+ r)))
             (aref a (1+ r)) (- (aref a r) (aref a (1+ r)))))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* 2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (* w^j (aref a r+j+m/2))))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (- u v))))))))

  a)

(defun dit-reverse (a)
  "Compute the radix-2 decimation-in-time inverse FFT of the complex vector A.

Input must be in bit-reversed order.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (bit-reversed-permute! a)
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (/ (+ (aref a r) (aref a (1+ r))) N)
             (aref a (1+ r)) (/ (- (aref a r) (aref a (1+ r))) N)))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* -2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (* w^j (aref a r+j+m/2))))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (- u v))))))))

  a)


#+#:DOESNT-WORK
(defun dif-forward (f)
  (labels ((twiddle (k)
             (cis (/ (* -2 pi k) (length f))))
           (dif (start-e N)
             (unless (= N 1)
               (let* ((N/2 (floor N 2))
                      (start-o (+ start-e N/2)))
                 (loop :for k :below n/2
                       :for e+k := (+ start-e k)
                       :for o+k := (+ start-o k)
                       :for f_e+k := (aref f e+k)
                       :for f_o+k := (aref f o+k)
                       :for e := (+ f_e+k f_o+k)
                       :for o := (* (twiddle k)
                                    (- f_e+k f_o+k))
                       :do (setf (aref f e+k) e
                                 (aref f o+k) o))

                 (dif start-e N/2)
                 (dif start-o N/2)))))
    ;; Perform the FTT via in-pace Decimation in Frequency.
    (dif 0 (length f))

    ;; Bit reverse
    (bit-reversed-permute! f)

    ;; Return the modified array.
    f))

#+#:DOESNT-WORK
(defun dit-forward (f)
  (labels ((twiddle (k)
             (cis (/ (* -2 pi k) (length f))))
           (dit (start-t N)
             (unless (= N 1)
               (let* ((N/2 (floor N 2))
                      (start-b (+ start-t N/2)))
                 (dit start-t N/2)
                 (dit start-b N/2)
                 (loop :for k :below n/2
                       :for t+k := (+ start-t k)
                       :for b+k := (+ start-b k)
                       :for top := (aref f t+k)
                       :for bot := (* (aref f b+k) (twiddle k))
                       :do (setf (aref f t+k) (+ top bot)
                                 (aref f b+k) (- top bot)))))))

    ;; Bit reverse
    (bit-reversed-permute! f)

    ;; Perform the FTT via in-pace Decimation in Frequency.
    (dit 0 (length f))

    ;; Return the modified array.
    f))


;;;;;;;;;;;;;;; Demonstration Multiplication Algorithm ;;;;;;;;;;;;;;;

(defun digit-count (n)
  "How many decimal digits does it take to write N?"
  (if (zerop n)
      1
      (let* ((approx   (ceiling (integer-length n) (log 10.0d0 2)))
             (exponent (expt 10 (1- approx))))
        (if (> exponent n)
            (1- approx)
            approx))))

(defun least-power-of-two->= (n)
  "What is the least power-of-two greater than or equal to N?"
  (if (power-of-two-p n)
      n
      (ash 1 (integer-length n))))

(defun digits (n &key (size (digit-count n)))
  "Make an array of the decimal digits of N."
  (loop :with v := (make-array size :initial-element 0 :element-type 'digit)
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

(defmacro with-timed-region ((var) &body body)
  "Time the execution of the code CODE in internal time units, and increment VAR by that amount."
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (incf ,var (- (get-internal-real-time) ,start))))))

;;; A convolution whose length-N input sequences have a maximum value
;;; of M will have values bounded by N*M^2. Our prime must be larger
;;; than this.
(defun multiply (a b &key (verbose t))
  "Multiply the integers A and B using NTT's."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         ;;(length (least-power-of-two->= (* 2 (max a-count b-count))))
         (length (least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length))
         (m (first (find-suitable-moduli (max length (expt 10 2)))))
         (w (find-primitive-root length m))
         (runtime 0))
    (when verbose
      (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

      (format t "A's digits: ~A~%" a-digits)
      (format t "B's digits: ~A~%" b-digits))

    (with-timed-region (runtime)
      (setf a-digits (ntt-forward a-digits :modulus m :primitive-root w))
      (setf b-digits (ntt-forward b-digits :modulus m :primitive-root w)))

    (when verbose
      (format t "NTT(A): ~A~%" a-digits)
      (format t "NTT(B): ~A~%" b-digits))

    (with-timed-region (runtime)
      (setf a-digits (map-into a-digits (lambda (a b) (m* a b m)) a-digits b-digits)))

    (when verbose
      (format t "C = NTT(A)*NTT(B) mod ~D = ~A~%" m a-digits))

    (with-timed-region (runtime)
      (setf a-digits (ntt-reverse a-digits :modulus m :primitive-root w)))

    (when verbose
      (format t "NTT^-1(C): ~A~%" a-digits))

    (values
     (undigits a-digits)
     runtime)))

(defun fft-multiply (a b)
  "Multiply two non-negative integers A and B using FFTs."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (least-power-of-two->= (+ 1 a-count b-count)))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length)))
    (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))

    (format t "A's digits: ~A~%" a-digits)
    (format t "B's digits: ~A~%" b-digits)

    (setf a-digits (dif-forward a-digits))
    (setf b-digits (dif-forward b-digits))

    (format t "FFT(A): ~A~%" a-digits)
    (format t "FFT(B): ~A~%" b-digits)

    (setf a-digits (map-into a-digits (lambda (a b) (* a b)) a-digits b-digits))

    (format t "C = FFT(A)*FFT(B) = ~A~%" a-digits)

    (setf a-digits (dif-reverse a-digits))

    (format t "FFT^-1(C): ~A~%" a-digits)

    (undigits (map 'vector (lambda (z)
                             (round (realpart z)))
                   a-digits))))

;;;;;;;;;;;;;;;;;;;;; Chinese Remainder Theorem ;;;;;;;;;;;;;;;;;;;;;;

(defun chinese-remainder (congruence-equations)
  "Solves the set of relations

    x = a1  (mod m1)
    x = a2  (mod m2)
     ...
    x = aN  (mod mN)

for x. The congruences are encoded in the list of conses

    CONGRUENCES-EQUATIONS = ((a1 . m1) (a2 . m2) ... (aN . mN)).

All moduli must be pairwise coprime. Note that this requirement is satisfied by primes or powers of primes.

Returns two values: x and the composite modulus m1 * m2 * ... * mN."
  (loop :with composite-modulus := (reduce #'* congruence-equations :key #'cdr :initial-value 1)
        :for (a . m) :in congruence-equations
        :for complement-modulus := (floor composite-modulus m)
        :sum (* a
                (inv-mod complement-modulus m)
                complement-modulus)
          :into sum
        :finally (return (values (mod sum composite-modulus)
                                 composite-modulus))))
