;;;; number-theoretic-transform.lisp
;;;;
;;;; Copyright (c) 2014-2019 Robert Smith

(in-package #:hypergeometrica)

(defun %make-ntt-work (raw-storage length modulus)
  (declare (type raw-storage raw-storage)
           (type alexandria:array-length length)
           (type modulus modulus))
  #-hypergeometric-safe
  (declare (optimize speed (safety 0) (space 0)))
  #+hypergeometrica-safe
  (assert (and (>= length (length raw-storage))
               (power-of-two-p length)))
  (let* ((a (make-storage length))
         (raw-a (raw-storage-of-storage a)))
    ;; NB. LENGTH is the total power-of-two length, not
    ;; the length of the mpz!
    (dotimes (i (length raw-storage) a)
      (setf (aref raw-a i) (mod (aref raw-storage i) modulus)))))

(defun make-ntt-work (mpz length moduli)
  (declare (type mpz mpz)
           (type alexandria:array-length length)
           (type (simple-array digit (*)) moduli))
  (when *verbose*
    (format t "Allocating..."))
  (let ((start-time (get-internal-real-time)))
    (prog1 (loop :for m :of-type modulus :across moduli
                 :collect (%make-ntt-work (raw-storage mpz) length m))
      (when *verbose*
        (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))))))

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
           (inline m+ m- m*/fast m*/fast2 m*/fast2-unreduced)
           (optimize speed (safety 0) debug (space 0) (compilation-speed 0)))
  #+hypergeometrica-safe
  (assert (power-of-two-p (length a)))
  (let* ((m     (aref (scheme-moduli scheme)   mod-num))
         (m~    (aref (scheme-inverses scheme) mod-num))
         (N     (length a))
         (ln    (lg N))
         (roots (scheme-primitive-roots scheme)))
    (declare (type modulus m m~)
             (type alexandria:array-length N)
             (type alexandria:non-negative-fixnum ln))
    #+hypergeometrica-paranoid
    (assert (every (lambda (x) (< x m)) a))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (dw (aref roots ln mod-num (- ln lsubn) 0))
             (dw~ (aref roots ln mod-num (- ln lsubn) 1))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (declare (type alexandria:array-index r+j r+j+subn/2))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m*/fast w^j (m- u v m) m m~))))
          (setf w^j (m*/fast2 dw dw~ w^j m)))))

    (when (plusp ln)
      (loop :for r :below N :by 2 :do
        (symbol-macrolet ((u (aref a r))
                          (v (aref a (1+ r))))
          (psetf u (m+ u v m)
                 v (m- u v m))))))

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
           (inline m+ m- m* m*/fast m*/fast2 m*/fast2-unreduced)
           (optimize speed (safety 0) debug (space 0) (compilation-speed 0)))
  (let* ((m     (aref (scheme-moduli scheme)   mod-num))
         (m~    (aref (scheme-inverses scheme) mod-num))
         (N     (length a))
         (ldn   (lg N))
         (roots (scheme-inverse-primitive-roots scheme)))
    #+hypergeometrica-safe
    (assert (power-of-two-p (length a)))
    #+hypergeometrica-paranoid
    (assert (every (lambda (x) (< x m)) a))
    (let ((1/N  (aref (scheme-inverse-transform-lengths scheme) ldn mod-num 0))
          (1/N~ (aref (scheme-inverse-transform-lengths scheme) ldn mod-num 1)))
      #+hypergeometrica-paranoid
      (assert (= 1/N (inv-mod N m)))
      (when (plusp ldn)
        (loop :for r :below N :by 2 :do
          (symbol-macrolet ((u (aref a r))
                            (v (aref a (1+ r))))
            (psetf u (m*/fast2 1/N 1/N~ (m+ u v m) m)
                   v (m*/fast2 1/N 1/N~ (m- u v m) m))))))
    (loop :for ldm :from 2 :to ldn :do
      (let* ((subn (ash 1 ldm))
             (subn/2 (floor subn 2))
             (dw (aref roots ldn mod-num (- ldn ldm) 0))
             (dw~ (aref roots ldn mod-num (- ldn ldm) 1))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (m*/fast w^j (aref a r+j+subn/2) m m~)))
              (declare (type alexandria:array-index r+j r+j+subn/2))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m- u v m))))
          (setf w^j (m*/fast2 dw dw~ w^j m))))))

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
