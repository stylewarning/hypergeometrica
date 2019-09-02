;;;; fft-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype complex-double ()
  '(complex double-float))

(deftype fft-work ()
  '(simple-array complex-double (*)))

(declaim (type (member 2 4 8 16 32)
               +fft-coefficient-bits+
               +fft-overhead-factor+))
(defconstant +fft-coefficient-bits+ 16)
(defconstant +fft-overhead-factor+ (/ 64 +fft-coefficient-bits+))

(defun make-fft-work (mpz result-size)
  (declare (type mpz mpz)
           (type alexandria:array-length result-size))
  (let* ((raw-mpz (raw-storage mpz))
         (size (mpz-size mpz))
         ;; FFTs will use digits that are 16-bits! So quadruple the size.
         (fft-length (* +fft-overhead-factor+ result-size))
         (work (make-array fft-length :element-type 'complex-double :initial-element #C(0.0d0 0.0d0))))
    (declare (type fft-work work))
    (dotimes (i size work)
      (let ((digit (aref raw-mpz i)))
        (dotimes (k +fft-overhead-factor+)
          (let* ((digit-part (ldb (byte +fft-coefficient-bits+ (* k +fft-coefficient-bits+)) digit)))
            (declare (type digit digit)
                     (type (unsigned-byte #.+fft-coefficient-bits+) digit-part))
            (setf (aref work (+ (* +fft-overhead-factor+ i) k)) (coerce digit-part 'complex-double))))))))

;;;   length-bits + 2*transform-bits <= 50
(defconstant +fft-length-limit+ (2^ (- 50 (* 2 +fft-coefficient-bits+))))

(defun mpz-*/fft (x y)
  (declare (optimize speed))
  (let* ((size (+ (mpz-size x) (mpz-size y)))
         (length (least-power-of-two->= (1+ size))) ; Enough room for a carry
         (fft-x (make-fft-work x length))
         (fft-y (make-fft-work y length))
         (result (make-storage length)) 
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (when *verbose*
                            (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                            (setf start-time (get-internal-real-time))
                            (finish-output))))))
    (declare (type fft-work fft-x fft-y)
             (type storage result)
             (type alexandria:array-length size length))
    ;; The double-float mantissa is 52 bits, and we want 2 bits for
    ;; correct rounding, leaving us a limit of 50 bits.
    (when (> length +fft-length-limit+)
      (error "Transform of length ~D will overflow a 52-bit mantissa. Sorry." length))
    (when *verbose*
      (format t "~&Size: ~D (approx ~D decimal~:P)~%"
              size
              (round (* size $digit-bits)
                     (log 10.0d0 2.00)))
      (format t "Transform length (~D-bit~:P): ~D (x2 = ~D MiB)~%"
              +fft-coefficient-bits+
              #1=(* +fft-overhead-factor+ length)
              (round (/ (* 128 #1#) 8 1024 1024)))

      (format t "Forward..."))
    (with-parallel-work ()
      ;; These also work with
      ;;
      ;;   (dif-forward fft-x)
      ;;   (dif-forward fft-y)
      (with-task ()
        (napa-fft:fft fft-x :dst fft-x :in-order nil))
      (with-task ()
        (napa-fft:fft fft-y :dst fft-y :in-order nil)))
    (funcall report-time)

    ;; Pointwise multiply. The NTT work for X is mutated.
    (when *verbose*
      (format t "Pointwise multiply..."))
    (#+hypergeometrica-parallel lparallel:pdotimes
     #-hypergeometrica-parallel dotimes (i (* 4 length))
      (setf (aref fft-x i) (* (aref fft-x i) (aref fft-y i))))
    (funcall report-time)

    ;; Inverse transform
    (when *verbose*
      (format t "Reverse..."))
    ;; This also works with
    ;;
    ;;   (dit-reverse fft-x)
    (napa-fft:ifft fft-x :dst fft-x :in-order nil)
    (funcall report-time)
    
    (when *verbose*
      (loop :for z :across fft-x
            :maximize (realpart z) :into re
            :maximize (imagpart z) :into im
            :maximize (abs z)      :into a
            :finally (progn
                       (format t "theory  = ~A~%" (* length (expt (1- (2^ 16)) 2)))
                       (format t "max(re) = ~A~%" re)
                       (format t "max(im) = ~A~%" im)
                       (format t "max(ab) = ~A~%" a))))

    ;; Unpack the result. This is effectively done by doing something
    ;; like:
    ;;
    ;;     extract(X, 0 == i mod (+fft-overhead-factor+))) << 0 * (+fft-coefficient-bits+)
    ;;   + extract(X, 1 == i mod (+fft-overhead-factor+)) << 1 * (+fft-coefficient-bits+)
    ;;   + extract(X, 2 == i mod (+fft-overhead-factor+)) << 2 * (+fft-coefficient-bits+)
    ;;   + extract(X, 3 == i mod (+fft-overhead-factor+)) << 3 * (+fft-coefficient-bits+)
    ;;   + ...
    ;;
    ;; We use the addition routine to take care of carries for us.
    (when *verbose*
      (format t "Unpacking..."))
    (let ((temp (raw-storage-of-storage (make-storage (length result)))))
      (declare (type raw-storage temp))
      ;; First, copy all of the 0 (mod 4) indexed elements into the
      ;; result array.
      ;;
      ;; These coefficients are designed to make the full use of the
      ;; floating point mantissa So we unpack into 64-bit words.
      ;;
      ;; We don't lift this LET out because resizing may occur.
      (let ((raw (raw-storage-of-storage result)))
        (#+hypergeometrica-parallel lparallel:pdotimes
         #-hypergeometrica-parallel dotimes (i length)
         (let ((coef (aref fft-x (* +fft-overhead-factor+ i))))
           (declare (type complex-double coef))
           #+hypergeometrica-safe
           (unless (< (imagpart coef) 5.0d0)
             (error "Bad imaginary part x[~D]: ~A" (* +fft-overhead-factor+ i) coef))
           (let ((re-coef (sb-ext:truly-the (signed-byte 64) (round (realpart coef)))))
             (declare (type (unsigned-byte 64) re-coef))
             (setf (aref raw i) re-coef)))))
      ;; Now we copy all of the other elements and add them into our array.
      (do-range (k 1 +fft-overhead-factor+)
        (declare (type (integer 1 #.+fft-overhead-factor+) k))
        (when (> k 1)
          (fill temp 0))

        (let* ((hi-bits (* +fft-coefficient-bits+ k))
               (lo-bits (- 64 hi-bits)))
          (declare (type (integer (0) (64)) lo-bits hi-bits))
          (#+hypergeometrica-parallel lparallel:pdotimes
           #-hypergeometrica-parallel dotimes (i size)
           (declare (type alexandria:array-index i))
           (let ((coef (aref fft-x (+ k (sb-ext:truly-the alexandria:array-index (* +fft-overhead-factor+ i))))))
             (declare (type complex-double coef))
             #+hypergeometrica-safe
             (unless (< (imagpart coef) 5.0d0)
               (error "Bad imaginary part x[~D]: ~A"
                      (+ k (* +fft-overhead-factor+ i))
                      (imagpart coef))) 
             (let* ((re-coef (sb-ext:truly-the (signed-byte 64) (round (realpart coef))))
                    (lo (ldb (byte lo-bits 0) re-coef))
                    (hi (ldb (byte hi-bits lo-bits) re-coef)))
               (declare (type (unsigned-byte 64) re-coef)
                        (type (unsigned-byte 64) lo hi))
               ;; SBCL wouldn't optimize DPB because it didn't believe
               ;; me.
               (setf (aref temp i)      (logior (aref temp i)
                                                (sb-ext:truly-the digit (ash lo hi-bits)))
                     (aref temp (1+ i)) hi)))))
        ;; Add them together
        (%add-storages/unsafe result (raw-storage-of-storage result)
                              (length (raw-storage-of-storage result))
                              temp
                              (length temp))))
    (funcall report-time)
    (make-mpz (* (sign x) (sign y)) result)))
