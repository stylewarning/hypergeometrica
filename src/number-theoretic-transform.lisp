;;;; number-theoretic-transform.lisp
;;;;
;;;; Copyright (c) 2014-2019 Robert Smith

(in-package #:hypergeometrica)

;;;;;;;;;;;;;;;;;;;;; Number-Theoretic Transform ;;;;;;;;;;;;;;;;;;;;;

;;; Decimation-in-frequency algorithm.

(defun ntt-forward (a scheme mod-num)
  "Compute the forward number-theoretic transform of the array of integers A, with modulus M and primitive root W. If they are not provided, a suitable one will be computed.

M and W are extracted from the MODULAR-SCHEME SCHEME based off of their index MOD-NUM.

The array must have a power-of-two length.

The resulting array (a mutation of the input) will be in bit-reversed order."
  (declare (type raw-storage a)
           (type modular-scheme scheme)
           (type alexandria:array-index mod-num)
           (inline m+ m- m*/fast)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let* ((m     (aref (scheme-moduli scheme)   mod-num))
         (m-inv (aref (scheme-inverses scheme) mod-num))
         (N     (length a))
         (ln    (lg N))
         (roots (scheme-primitive-roots scheme)))
    (declare (type modulus m m-inv)
             (type alexandria:array-length N)
             (type alexandria:non-negative-fixnum ln))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (dw (aref roots ln mod-num (- ln lsubn)))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (declare (type alexandria:array-index r+j r+j+subn/2))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m*/fast w^j (m- u v m) m m-inv))))
          (setf w^j (m*/fast dw w^j m m-inv)))))

    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m+ (aref a r) (aref a (1+ r)) m)
             (aref a (1+ r)) (m- (aref a r) (aref a (1+ r)) m))))

  a)

;;; Decimation-in-time algorithm.
(defun ntt-reverse (a scheme mod-num)
  "Compute the inverse number-theoretic transform of the array of integers A, with modulus M and primitive root W. If they are not provided, a suitable one will be computed.

M and W are extracted from the MODULAR-SCHEME SCHEME based off of their index MOD-NUM.

The array must have a power-of-two length.

The input must be in bit-reversed order."
  (declare (type raw-storage a)
           (type modular-scheme scheme)
           (type alexandria:array-index mod-num)
           (inline m+ m- m* m*/fast)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let* ((m     (aref (scheme-moduli scheme)   mod-num))
         (m-inv (aref (scheme-inverses scheme) mod-num))
         (N     (length a))
         (ldn   (lg N))
         (roots (scheme-inverse-primitive-roots scheme))
         (1/N   (inv-mod N m)))
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m*/fast 1/N (m+ (aref a r) (aref a (1+ r)) m) m m-inv)
             (aref a (1+ r)) (m*/fast 1/N (m- (aref a r) (aref a (1+ r)) m) m m-inv)))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((subn (ash 1 ldm))
             (subn/2 (floor subn 2))
             (dw (aref roots ldn mod-num (- ldn ldm)))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (m*/fast w^j (aref a r+j+subn/2) m m-inv)))
              (declare (type alexandria:array-index r+j r+j+subn/2))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m- u v m))))
          (setf w^j (m*/fast dw w^j m m-inv))))))

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

  a)

(defun dit-forward (a)
  "Compute the radix-2 decimation-in-time FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
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
