;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun count-trailing-zeroes (n)
  (assert (plusp n))
  (loop :for z :from 0
        :for x := n :then (ash x -1)
        :while (evenp x)
        :finally (return z)))

(defparameter *moduli*
  (sort (remove-duplicates
         (remove-if-not (lambda (m)
                          (<= 58 (integer-length m) (1- $digit-bits)))
                        (loop :for len :from 54 :to 60
                              :append (find-suitable-moduli (expt 2 len) :count 100))))
        #'>)
  "A set of moduli used to do transforms. They are sorted in decreasing order.")

;;; TODO: check that the transform lengths are compatible with this
(defparameter *max-transform-length-bits*
  (alexandria:extremum (mapcar (alexandria:compose #'count-trailing-zeroes #'1-) *moduli*) #'<)
  "The maximum lg(size) of a transform.")


(defun moduli-for-bits (bits)
  (labels ((get-em (moduli-collected moduli-left bits-remaining)
             (cond
               ((plusp bits-remaining)
                (assert (not (endp moduli-left)))
                (let ((mod (first moduli-left)))
                  (get-em (cons mod moduli-collected)
                          (rest moduli-left)
                          (- bits-remaining (integer-length mod)))))
               (t
                (nreverse moduli-collected)))))
    (get-em nil *moduli* bits)))

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
  (assert (plusp n))
  (if (= 1 n)
      x
      (iterate f (funcall f x) (1- n))))

(defun make-ntt-work (mpz length moduli)
  (loop :for m :in moduli
        :for a := (make-storage length)
        :for raw-a := (raw-storage-of-storage a)
        :do (map-into raw-a (modder m) (storage mpz))
        :collect a))

(defun mpz-square (x)
  (let* ((size (mpz-size x))
         (length (least-power-of-two->= (* 2 size)))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (moduli (moduli-for-bits bound-bits))
         (roots (loop :for m :in moduli
                      :collect (find-primitive-root length m)))
         (ntts (make-ntt-work x length moduli))
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
      (format t "Moduli: ~{#x~16X~^, ~}~%" moduli)

      (format t "Forward"))
    (loop :for m :in moduli
          :for w :in roots
          :for a :in raw-ntts
          :do (ntt-forward a m w)
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Pointwise multiply
    (when *verbose*
      (format t "Pointwise multiply"))
    (loop :for m :in moduli
          :for a :in raw-ntts
          :do (dotimes (i length)
                (let ((ai (aref a i)))
                  (setf (aref a i) (m* ai ai m))))
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse"))
    (loop :for m :in moduli
          :for w :in roots
          :for a :in raw-ntts
          :do (ntt-reverse a m w)
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Unpack the result.
    (when *verbose*
      (format t "CRT..."))
    (let* ((composite   (reduce #'* moduli))
           (complements (mapcar (lambda (m) (/ composite m)) moduli))
           (inverses    (mapcar #'inv-mod complements moduli))
           (factors     (mapcar #'* complements inverses))
           (raw-result  (raw-storage-of-storage result)))
      (dotimes (i length)
        (loop :for a :in raw-ntts
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) raw-result i)))
      (funcall report-time))
    (make-mpz 1 result)))

(defun mpz-* (x y)
  (let* ((size (+ (mpz-size x) (mpz-size y)))
         (length (least-power-of-two->= size))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (moduli (moduli-for-bits bound-bits))
         (roots (loop :for m :in moduli
                      :collect (find-primitive-root length m)))
         (ntts-x (make-ntt-work x length moduli))
         (raw-ntts-x (mapcar #'raw-storage-of-storage ntts-x))
         (ntts-y (make-ntt-work y length moduli))
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
      (format t "Moduli: ~{#x~16X~^, ~}~%" moduli)

      (format t "Forward"))
    (loop :for m :in moduli
          :for w :in roots
          :for ax :in raw-ntts-x
          :for ay :in raw-ntts-y
          :do (ntt-forward ax m w)
              (when *verbose*
                (write-char #\.))
              (ntt-forward ay m w)
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Pointwise multiply. The NTT work for X is mutated.
    (when *verbose*
      (format t "Pointwise multiply"))
    (loop :for m :in moduli
          :for ax :in raw-ntts-x
          :for ay :in raw-ntts-y
          :do (dotimes (i length)
                (setf (aref ax i) (m* (aref ax i) (aref ay i) m)))
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Tell the garbage collector we don't need no vectors anymore.
    (setf ntts-y nil)
    (fill (raw-storage-of-storage result) 0)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse"))
    (loop :for m :in moduli
          :for w :in roots
          :for ax :in raw-ntts-x
          :do (ntt-reverse ax m w)
              (when *verbose*
                (write-char #\.)))
    (funcall report-time)

    ;; Unpack the result.
    (when *verbose*
      (format t "CRT..."))
    (let* ((composite   (reduce #'* moduli))
           (complements (mapcar (lambda (m) (/ composite m)) moduli))
           (inverses    (mapcar #'inv-mod complements moduli))
           (factors     (mapcar #'* complements inverses))
           (raw-result  (raw-storage-of-storage result)))
      (dotimes (i length)
        (loop :for a :in ntts-x
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) raw-result i)))
      (funcall report-time))
    (make-mpz (* (sign x) (sign y)) result)))
