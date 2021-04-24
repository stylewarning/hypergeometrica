;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)


(global-vars:define-global-parameter **scheme** (make-modular-scheme (default-moduli)))

(defun add-big-digit (big-digit storage i)
  "Add the BIG-DIGIT (an UNSIGNED-BYTE) to the STORAGE (a RAW-STORAGE) beginning at the Ith digit."
  (declare (type unsigned-byte big-digit)
           (type storage storage)
           (type alexandria:array-index i)
           (optimize speed))
  (with-vec (storage storage_)
    (cond
      ((zerop big-digit) storage)
      ((>= i (vec-digit-length storage)) (error "Trying to add ~D at index ~D. ~
                                                 This is unexpected and indicates ~
                                                 a grave inconsistency."
                                                big-digit i))
      ((typep big-digit 'fixnum)
       (multiple-value-bind (x carry) (add64 big-digit (storage_ i))
         (declare (type bit carry))
         (setf (storage_ i) x)
         (add-big-digit carry storage (1+ i))))
      ;; Specially written to not cons.
      #+sbcl
      (t (dotimes (j (sb-bignum:%bignum-length big-digit) storage)
           (let* ((offset (+ i j))
                  (si (storage_ offset))
                  (bi (sb-bignum:%bignum-ref big-digit j)))
             (declare (type alexandria:array-index offset)
                      (type (unsigned-byte 64) si bi))
             (multiple-value-bind (x carry) (add64 si bi)
               (setf (storage_ offset) x)
               (add-big-digit carry storage (1+ offset))))))
      ;; Otherwise, we gotta cons...
      #-sbcl
      (t (let ((si (storage_ i))
               (digit-lo64 (ldb (byte $digit-bits 0) big-digit)))
           (declare (type (unsigned-byte 64) si digit-lo64))
           (multiple-value-bind (x carry) (add64 si digit-lo64)
             (declare (type bit carry))
             (setf (storage_ i) x)
             (let ((quo (ash big-digit #.(- $digit-bits))))
               ;; Do QUO + CARRY in two separate steps to avoid a bignum
               ;; addition on the Lisp side.
               (add-big-digit quo storage (1+ i))
               (add-big-digit carry storage (1+ i)))))))))

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

(defun multiply-pointwise! (a b length scheme i)
  (declare (type storage a b)
           (type alexandria:array-length length)
           (type alexandria:array-index i)
           (type modular-scheme scheme)
           (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (with-vecs (a a_ b b_)
    (let ((m (aref (scheme-moduli scheme) i))
          (m~ (aref (scheme-inverses scheme) i)))
      (#+hypergeometrica-parallel lparallel:pdotimes
       #-hypergeometrica-parallel dotimes (i length)
       (setf (a_ i) (m*/fast (a_ i) (b_ i) m m~))))))

(defun mpz-square (x)
  (let* ((size (mpz-size x))
         (length (least-power-of-two->= (* 2 size)))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (num-moduli (num-moduli-needed-for-bits **scheme** bound-bits))
         (ntts (make-ntt-work x length (scheme-moduli **scheme**)))
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
            :for a :in ntts
            :do (with-task (a i)
                  (ntt-forward a **scheme** i))))
    (funcall report-time)

    ;; Pointwise multiply
    (when *verbose*
      (format t "Pointwise multiply... "))
    (loop :for i :below num-moduli
          :for m := (aref (scheme-moduli **scheme**) i)
          :for mi := (aref (scheme-inverses **scheme**) i)
          :for a :in ntts
          :do (with-vec (a a_)
                (#+hypergeometrica-parallel lparallel:pdotimes
                 #-hypergeometrica-parallel dotimes (i length)
                 (let ((ai (a_ i)))
                   (setf (a_ i) (m*/fast ai ai m mi))))))
    (funcall report-time)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    (with-parallel-work ()
      (loop :for i :below num-moduli
            :for a :in ntts
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
           (factors     (map 'list #'* complements inverses)))
      (dotimes (i length)
        (loop :for a :in ntts
              :for f :in factors
              ;; TODO: optimize
              :sum (* f (vec-ref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) result i)))
      (funcall report-time))
    (make-mpz 1 result)))

(defun mpz-*/ntt (x y)
  (let* ((size (+ (mpz-size x) (mpz-size y)))
         (length (least-power-of-two->= size))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (num-moduli (num-moduli-needed-for-bits **scheme** bound-bits))
         (ntts-x (make-ntt-work x length (scheme-moduli **scheme**)))
         (ntts-y (make-ntt-work y length (scheme-moduli **scheme**)))
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
            :for ax :in ntts-x
            :for ay :in ntts-y
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
            :for ax :in ntts-x
            :for ay :in ntts-y
            :do (with-task (i ax ay)
                  (multiply-pointwise! ax ay length **scheme** i))))
    (funcall report-time)

    ;; Tell the garbage collector we don't need no vectors anymore.
    (setf ntts-y nil)
    (vec-fill result 0)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    (with-parallel-work ()
      (loop :for i :below num-moduli
            :for ax :in ntts-x
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
           (factors     (map 'list #'* complements inverses)))
      (dotimes (i length)
        (loop :for a :in ntts-x
              :for f :in factors
              ;; TODO: optimize
              :sum (* f (vec-ref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) result i)))
      (funcall report-time))
    (make-mpz (* (sign x) (sign y)) result)))



