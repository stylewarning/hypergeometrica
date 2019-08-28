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

;;; We use a separate data type for NTTs because they can in princple
;;; use a different (perhaps smaller!) coefficient domain.

(deftype ntt-coefficient ()
  'digit)

(deftype ntt-array ()
  '(simple-array ntt-coefficient (*)))

(defun make-ntt-array (n)
  (make-array n :element-type 'ntt-coefficient
                :initial-element 0))

(declaim (ftype (function (ntt-array) storage) ntt-array-to-storage))
(defun ntt-array-to-storage (x)
  ;; This MUST match up with MAKE-STORAGE.
  (make-array (length x) :element-type 'digit
                         :adjustable t
                         :displaced-to x))


;;; Decimation-in-frequency algorithm.
(defun ntt-forward (a m w)
  "Compute the forward number-theoretic transform of the array of integers A, with modulus M and primitive root W. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length.

The resulting array (a mutation of the input) will be in bit-reversed order."
  (declare (type ntt-array a)
           (type modulus m)
           (type ntt-coefficient w))
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
(defun ntt-reverse (a m w)
  "Compute the inverse number-theoretic transform of the array of integers A, with modulus M and primitive root W. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length.

The input must be in bit-reversed order."
  (declare (type ntt-array a)
           (type modulus m)
           (type ntt-coefficient w))
  (let* ((1/w (inv-mod w m))
         (N   (length a))
         (1/N (inv-mod N m))
         (ldn (1- (integer-length N))))
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m* 1/N (m+ (aref a r) (aref a (1+ r)) m) m)
             (aref a (1+ r)) (m* 1/N (m- (aref a r) (aref a (1+ r)) m) m)))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((subn (ash 1 ldm))
             (subn/2 (floor subn 2))
             (dw (expt-mod 1/w (ash 1 (- ldn ldm)) m))
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
