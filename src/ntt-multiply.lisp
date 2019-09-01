;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)


(global-vars:define-global-parameter **scheme** (make-modular-scheme (default-moduli)))

(defun modder (m)
  (lambda (n)
    (mod n m)))

(defun add-big-digit (digit storage i)
  (cond
    ((zerop digit) storage)
    ((>= i (length storage)) (error "Trying to add ~D at index ~D" digit i))
    (t (let ((sum (+ digit (aref storage i))))
         (multiple-value-bind (quo rem) (floor sum $base)
           (setf (aref storage i) rem)
           (add-big-digit quo storage (1+ i)))))))

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
  (loop :for m :across moduli
        :for a := (make-storage length)
        :for raw-a := (raw-storage-of-storage a)
        :do (map-into raw-a (modder m) (storage mpz))
        :collect a))

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
      (format t "~&~%Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
              size
              (round (* size $digit-bits)
                     (log 10.0d0 2.00))
              (round (/ (* size $digit-bits) 8 1024 1024)))
      (format t "Transform length: ~D~%" length)
      (format t "Convolution bits: ~D~%" bound-bits)
      (format t "Moduli: ~{#x~16X~^, ~}~%" (coerce (scheme-moduli **scheme**) 'list))

      (format t "Forward..."))
    (loop :for i :below num-moduli
          :for a :in raw-ntts
          :do (ntt-forward a **scheme** i))
    (funcall report-time)

    ;; Pointwise multiply
    (when *verbose*
      (format t "Pointwise multiply... "))
    (loop :for i :below num-moduli
          :for m := (aref (scheme-moduli **scheme**) i)
          :for mi := (aref (scheme-inverses **scheme**) i)
          :for a :in raw-ntts
          :do (dotimes (i length)
                (let ((ai (aref a i)))
                  (setf (aref a i) (m*/fast ai ai m mi)))))
    (funcall report-time)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    (loop :for i :below num-moduli
          :for a :in raw-ntts
          :do (ntt-reverse a **scheme** i))
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

(defparameter *ntt-multiply-threshold* 100
  "Up to how many digits can the smaller number of a multiplication have before NTT multiplication is used?")

(defun mpz-* (x y)
  (optimize-storage x)
  (optimize-storage y)
  (when (< (mpz-size x) (mpz-size y))
    (rotatef x y))
  ;; Now the size of X is guaranteed greater-or-equal Y.
  (cond
    ((= 1 (mpz-size y))
     (let ((d (aref (storage y) 0))
           (r (make-mpz (* (sign x) (sign y))
                        (make-storage (+ 2 (mpz-size x))))))
       (replace (raw-storage r) (raw-storage x))
       (mpz-multiply-by-digit! d r)
       (optimize-storage r)
       r))
    ((<= (mpz-size y) *ntt-multiply-threshold*)
     (let ((r-storage (%multiply-storage/schoolboy
                       (raw-storage x) (mpz-size x)
                       (raw-storage y) (mpz-size y))))
       (make-mpz (* (sign x) (sign y)) r-storage)))
    (t
     (mpz-*/ntt x y))))

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
      (format t "~&~%Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
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
