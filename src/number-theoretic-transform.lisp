;;;; number-theoretic-transform.lisp
;;;;
;;;; Copyright (c) 2014-2019 Robert Smith

(in-package #:hypergeometrica)

;;;;;;;;;;;;;;;;;;;;; Number-Theoretic Transform ;;;;;;;;;;;;;;;;;;;;;

;;; Decimation-in-frequency algorithm.
(defun ntt-forward (a m w)
  "Compute the forward number-theoretic transform of the array of integers A, with modulus M and primitive root W. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length.

The resulting array (a mutation of the input) will be in bit-reversed order."
  (declare (type raw-storage a)
           (type modulus m)
           (type digit w)
           (inline m+ m- m*)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
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
              (declare (type alexandria:array-index r+j r+j+subn/2))
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
  (declare (type raw-storage a)
           (type modulus m)
           (type digit w)
           (inline m+ m- m* expt-mod)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
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
             (dw (expt-mod/2^n 1/w (- ldn ldm) m))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (m* w^j (aref a r+j+subn/2) m)))
              (declare (type alexandria:array-index r+j r+j+subn/2))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m- u v m))))
          (setf w^j (m* dw w^j m))))))

  a)

(defun multiply-pointwise! (a b length m)
  (declare (type raw-storage a b)
           (type alexandria:array-length length)
           (type modulus m)
           (inline m*)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (dotimes (i length)
    (setf (aref a i) (m* (aref a i) (aref b i) m))))


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
