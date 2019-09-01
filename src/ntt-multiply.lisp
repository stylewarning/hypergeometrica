;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)


(global-vars:define-global-parameter **scheme** (make-modular-scheme (default-moduli)))

(defun add-big-digit (big-digit storage i)
  (declare (type unsigned-byte big-digit)
           (type raw-storage storage)
           (type alexandria:array-index i)
           (optimize speed))
  (cond
    ((zerop big-digit) storage)
    ((>= i (length storage)) (error "Trying to add ~D at index ~D. ~
                                     This is unexpected and indicates ~
                                     a grave inconsistency."
                                    big-digit i))
    ((typep big-digit 'fixnum)
     (multiple-value-bind (x carry) (add64 big-digit (aref storage i))
       (declare (type bit carry))
       (setf (aref storage i) x)
       (add-big-digit carry storage (1+ i))))
    ;; Specially written to not cons.
    #+sbcl
    (t (dotimes (j (sb-bignum:%bignum-length big-digit) storage)
         (let* ((offset (+ i j))
                (si (aref storage offset))
                (bi (sb-bignum:%bignum-ref big-digit j)))
           (declare (type alexandria:array-index offset)
                    (type (unsigned-byte 64) si bi))
           (multiple-value-bind (x carry) (add64 si bi)
             (setf (aref storage offset) x)
             (add-big-digit carry storage (1+ offset))))))
    ;; Otherwise, we gotta cons...
    #-sbcl
    (t (let ((si (aref storage i))
             (digit-lo64 (ldb (byte $digit-bits 0) big-digit)))
         (declare (type (unsigned-byte 64) si digit-lo64))
         (multiple-value-bind (x carry) (add64 si digit-lo64)
           (declare (type bit carry))
           (setf (aref storage i) x)
           (let ((quo (ash big-digit #.(- $digit-bits))))
             ;; Do QUO + CARRY in two separate steps to avoid a bignum
             ;; addition on the Lisp side.
             (add-big-digit quo storage (1+ i))
             (add-big-digit carry storage (1+ i))))))))

(defun iterate (f x n)
  (assert (not (minusp n)))
  (if (zerop n)
      x
      (iterate f (funcall f x) (1- n))))


(defmacro with-rebind ((&rest vars) &body body)
  `(let ,(loop :for var :in vars :collect (list var var))
     ,@body))

(defmacro with-task ((&rest vars) &body work)
  `(with-rebind ,vars
     ,@work))

(defmacro with-parallel-work (() &body body)
  #+hypergeometrica-parallel
  (alexandria:with-gensyms (ch num-items i work vars)
    `(let ((,num-items 0)
           (,ch (lparallel:make-channel)))
       (declare (type fixnum ,num-items))
       (macrolet ((with-task ((&rest ,vars) &body ,work)
                    `(with-rebind ,,vars
                       (incf ,',num-items)
                       (lparallel:submit-task ,',ch
                                              (lambda ()
                                                ,@,work)))))
         ,@body)
       ;; do work
       (dotimes (,i ,num-items)
         (lparallel:receive-result ,ch))))
  #-hypergeometrica-parallel
  `(progn
     ,@body))

(defun make-ntt-work (mpz length moduli)
  (declare (type mpz mpz)
           (type alexandria:array-length length)
           (type (simple-array digit (*)) moduli))
  (when *verbose*
    (format t "Allocating..."))
  (let ((start-time (get-internal-real-time)))
    (prog1
        (let ((raw-mpz (raw-storage mpz)))
          #-hypergeometric-safe
          (declare (optimize speed (safety 0) (debug 0) (space 0)))
          (loop :for m :of-type modulus :across moduli
                :collect
                (let* ((a (make-storage length))
                       (raw-a (raw-storage-of-storage a)))
                  ;; NB. LENGTH is the total power-of-two length, not
                  ;; the length of the mpz!
                  (dotimes (i (length raw-mpz) a)
                    (setf (aref raw-a i) (mod (aref raw-mpz i) m))))))
      (when *verbose*
        (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))))))

(defun multiply-pointwise! (a b length scheme i)
  (declare (type raw-storage a b)
           (type alexandria:array-length length)
           (type alexandria:array-index i)
           (type modular-scheme scheme)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((m (aref (scheme-moduli scheme) i))
        (m-inv (aref (scheme-inverses scheme) i)))
    (#+hypergeometrica-parallel lparallel:pdotimes
     #-hypergeometrica-parallel dotimes (i length)
      (setf (aref a i) (m*/fast (aref a i) (aref b i) m m-inv)))))

(defun mpz-square (x)
  (let* ((size (mpz-size x))
         (length (least-power-of-two->= (* 2 size)))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (num-moduli (num-moduli-needed-for-bits **scheme** bound-bits))
         (ntts (make-ntt-work x length (scheme-moduli **scheme**)))
         (raw-ntts (mapcar #'raw-storage-of-storage ntts))
         ;; TODO don't allocate
         (result (make-storage length))
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (when *verbose*
                            (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                            (setf start-time (get-internal-real-time))
                            (finish-output))))))
    (when *verbose*
      (format t "~&Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
              size
              (round (* size $digit-bits)
                     (log 10.0d0 2.00))
              (round (/ (* (1+ num-moduli) length $digit-bits) 8 1024 1024)))
      (format t "Transform length: ~D~%" length)
      (format t "Convolution bits: ~D~%" bound-bits)
      (format t "Moduli: ~{#x~16X~^, ~}~%" (coerce (scheme-moduli **scheme**) 'list))

      (format t "Forward..."))
    (with-parallel-work ()
      (loop :for i :below num-moduli
            :for a :in raw-ntts
            :do (with-task (a i)
                  (ntt-forward a **scheme** i))))
    (funcall report-time)

    ;; Pointwise multiply
    (when *verbose*
      (format t "Pointwise multiply... "))
    (loop :for i :below num-moduli
          :for m := (aref (scheme-moduli **scheme**) i)
          :for mi := (aref (scheme-inverses **scheme**) i)
          :for a :in raw-ntts
          :do (#+hypergeometrica-parallel lparallel:pdotimes
               #-hypergeometrica-parallel dotimes (i length)
                (let ((ai (aref a i)))
                  (setf (aref a i) (m*/fast ai ai m mi)))))
    (funcall report-time)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    (with-parallel-work ()
      (loop :for i :below num-moduli
            :for a :in raw-ntts
            :do (with-task (a i)
                  (ntt-reverse a **scheme** i))))
    (funcall report-time)

    ;; Unpack the result.
    (when *verbose*
      (format t "CRT..."))
    (let* ((moduli      (subseq (scheme-moduli **scheme**) 0 num-moduli))
           (composite   (reduce #'* moduli))
           (complements (map 'list (lambda (m) (/ composite m)) moduli))
           (inverses    (map 'list #'inv-mod complements moduli))
           (factors     (map 'list #'* complements inverses))
           (raw-result  (raw-storage-of-storage result)))
      (dotimes (i length)
        (loop :for a :in raw-ntts
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) raw-result i)))
      (funcall report-time))
    (make-mpz 1 result)))

(defun mpz-*/ntt (x y)
  (let* ((size (+ (mpz-size x) (mpz-size y)))
         (length (least-power-of-two->= size))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (num-moduli (num-moduli-needed-for-bits **scheme** bound-bits))
         (ntts-x (make-ntt-work x length (scheme-moduli **scheme**)))
         (raw-ntts-x (mapcar #'raw-storage-of-storage ntts-x))
         (ntts-y (make-ntt-work y length (scheme-moduli **scheme**)))
         (raw-ntts-y (mapcar #'raw-storage-of-storage ntts-y))
         ;; By the time we write to RESULT, NTTS-Y will be done.
         ;;
         ;; However (!), we will need to remember to clear it.
         (result (first ntts-y))
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (when *verbose*
                            (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                            (setf start-time (get-internal-real-time))
                            (finish-output))))))
    (when *verbose*
      (format t "~&Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
              size
              (round (* size $digit-bits)
                     (log 10.0d0 2.00))
              (round (/ (* size $digit-bits) 8 1024 1024)))
      (format t "Transform length: ~D~%" length)
      (format t "Convolution bits: ~D~%" bound-bits)
      (format t "Moduli: ~{#x~16X~^, ~}~%" (coerce (scheme-moduli **scheme**) 'list))

      (format t "Forward..."))
    (with-parallel-work ()
     (loop :for i :below num-moduli
           :for ax :in raw-ntts-x
           :for ay :in raw-ntts-y
           :do (with-task (i ax)
                 (ntt-forward ax **scheme** i))
           :do (with-task (i ay)
                 (ntt-forward ay **scheme** i))))
    (funcall report-time)

    ;; Pointwise multiply. The NTT work for X is mutated.
    (when *verbose*
      (format t "Pointwise multiply..."))
    (with-parallel-work ()
     (loop :for i :below num-moduli
           :for ax :in raw-ntts-x
           :for ay :in raw-ntts-y
           :do (with-task (i ax ay)
                 (multiply-pointwise! ax ay length **scheme** i))))
    (funcall report-time)

    ;; Tell the garbage collector we don't need no vectors anymore.
    (setf ntts-y nil)
    (fill (raw-storage-of-storage result) 0)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    (with-parallel-work ()
     (loop :for i :below num-moduli
           :for ax :in raw-ntts-x
           :do (with-task (i ax)
                 (ntt-reverse ax **scheme** i))))
    (funcall report-time)

    ;; Unpack the result.
    ;;
    ;; This allocates a lot, but seems to be fast in practice.
    (when *verbose*
      (format t "CRT..."))
    (let* ((moduli      (subseq (scheme-moduli **scheme**) 0 num-moduli))
           (composite   (reduce #'* moduli))
           (complements (map 'list (lambda (m) (/ composite m)) moduli))
           (inverses    (map 'list #'inv-mod complements moduli))
           (factors     (map 'list #'* complements inverses))
           (raw-result  (raw-storage-of-storage result)))
      (dotimes (i length)
        (loop :for a :in raw-ntts-x
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) raw-result i)))
      (funcall report-time))
    (make-mpz (* (sign x) (sign y)) result)))

;;; Multiplication Driver

(defparameter *ntt-multiply-threshold* 100
  "Up to how many digits can the smaller number of a multiplication have before NTT multiplication is used?")

(defun mpz-* (x y)
  (optimize-storage x)
  (optimize-storage y)
  (when (< (mpz-size x) (mpz-size y))
    (rotatef x y))
  ;; Now the size of X is guaranteed greater-or-equal Y.
  (optimize-storage
   (cond
     ((mpz-zerop y)
      (integer-mpz 0))
     ((= 1 (mpz-size y))
      (let ((d (aref (storage y) 0)))
        (cond
          ((= 1 d)
           (if (= -1 (sign y))
               (mpz-negate x)
               x))
          (t
           (let ((r (make-mpz (* (sign x) (sign y))
                              (make-storage (+ 2 (mpz-size x))))))
             (replace (raw-storage r) (raw-storage x))
             (mpz-multiply-by-digit! d r)
             r)))))
     ((<= (mpz-size y) *ntt-multiply-threshold*)
      (let ((r-storage (%multiply-storage/schoolboy
                        (raw-storage x) (mpz-size x)
                        (raw-storage y) (mpz-size y))))
        (make-mpz (* (sign x) (sign y)) r-storage)))
     ((eq x y)
      (mpz-square x))
     (t
      (mpz-*/ntt x y)))))

