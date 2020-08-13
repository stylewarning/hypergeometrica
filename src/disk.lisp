;;;; disk.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun write-zeros (stream n)
  "Write N zeroes to the stream STREAM."
  (loop :repeat n :do (write-byte 0 stream)))

(defun write-mpz (stream mpz)
  (loop :with r := (raw-storage mpz)
        :for i :below (mpz-size mpz)
        :for x := (aref r i)
        :do (write-byte x stream)))

(defun disk-forward-mfa (fx scheme mod-num)
  (check-type fx file-backed-storage)
  (assert (power-of-two-p (storage-length fx)))
  (let* ((length (storage-length fx))
         (sx     (file-backed-storage-stream fx))
         (m (aref (scheme-moduli scheme) mod-num))
         (lgn (lg length))
         (w (aref (scheme-primitive-roots scheme) lgn mod-num 0 0))
         ;; factorize
         (rows (expt 2 (floor lgn 2)))
         (cols (expt 2 (- lgn (floor lgn 2))))
         ;; workspace
         (col-work (raw-storage-of-storage (make-storage rows)))
         (row-work (raw-storage-of-storage (make-storage cols))))
    (assert (= length (* rows cols)))
    ;;(format t "DISK-MFA: m=#x~X, w=~D, RxC = ~Dx~D~%" m w rows cols)
    (labels ((read-column! (c)
               (dotimes (r rows)
                 (seek sx (+ c (* r cols)))
                 (setf (aref col-work r) (read-byte sx))))
             (write-column! (c)
               (dotimes (r rows)
                 (seek sx (+ c (* r cols)))
                 (write-byte (aref col-work r) sx)))
             (read-row! (r)
               (seek sx (* r cols))
               (read-sequence row-work sx))
             (write-row! (r)
               (seek sx (* r cols))
               (write-sequence row-work sx)))
      ;; Apply a length R xform on each column
      (dotimes (c cols)
        (read-column! c)
        (map-into col-work (lambda (x) (mod x m)) col-work)
        (ntt-forward col-work scheme mod-num)
        (bit-reversed-permute! col-work)
        (write-column! c))
      ;; Multiply twiddle factors
      (dotimes (r rows)
        (read-row! r)
        (dotimes (c cols)
          (let ((w^rc (expt-mod/safe w (* r c) m)))
            (setf (aref row-work c) (m* w^rc (aref row-work c) m))))
        (write-row! r))
      ;; Apply a length C xform to each row
      (dotimes (r rows)
        (read-row! r)
        (ntt-forward row-work scheme mod-num)
        (bit-reversed-permute! row-work)
        (write-row! r)))))

(defun disk-reverse-mfa (fx scheme mod-num)
  (check-type fx file-backed-storage)
  (assert (power-of-two-p (storage-length fx)))
  (let* ((length (storage-length fx))
         (sx     (file-backed-storage-stream fx))
         (m (aref (scheme-moduli scheme) mod-num))
         (lgn (lg length))
         (w (aref (scheme-inverse-primitive-roots scheme) lgn mod-num 0 0))
         ;; factorize
         (rows (expt 2 (floor lgn 2)))
         (cols (expt 2 (- lgn (floor lgn 2))))
         ;; workspace
         (col-work (raw-storage-of-storage (make-storage rows)))
         (row-work (raw-storage-of-storage (make-storage cols))))
    (assert (= length (* rows cols)))
    ;;(format t "DISK-MFA: m=#x~X, w=~D, RxC = ~Dx~D~%" m w rows cols)
    (labels ((read-column! (c)
               (dotimes (r rows)
                 (seek sx (+ c (* r cols)))
                 (setf (aref col-work r) (read-byte sx))))
             (write-column! (c)
               (dotimes (r rows)
                 (seek sx (+ c (* r cols)))
                 (write-byte (aref col-work r) sx)))
             (read-row! (r)
               (seek sx (* r cols))
               (read-sequence row-work sx))
             (write-row! (r)
               (seek sx (* r cols))
               (write-sequence row-work sx)))
      ;; Apply a length C xform to each row
      (dotimes (r rows)
        (read-row! r)
        (bit-reversed-permute! row-work)
        (ntt-reverse row-work scheme mod-num)
        (write-row! r))
      ;; Multiply twiddle factors
      (dotimes (r rows)
        (read-row! r)
        (dotimes (c cols)
          (let ((w^rc (expt-mod/safe w (* r c) m)))
            (setf (aref row-work c) (m* w^rc (aref row-work c) m))))
        (write-row! r))
      ;; Apply a length R xform on each column
      (dotimes (c cols)
        (read-column! c)
        (bit-reversed-permute! col-work)
        (ntt-reverse col-work scheme mod-num)
        (write-column! c)))))

(defun map-into-disk! (f fa)
  (rewind-file-backed-storage fa)
  (let ((na (storage-length fa)))
    (let* ((sa (file-backed-storage-stream fa))
           (n-tmp (min na (expt 2 8)))
           (a-tmp (raw-storage-of-storage (make-storage n-tmp))))
      ;; TODO: no need to re-seek all the time. just read linearly.
      (dotimes (k (floor na n-tmp))
        (seek sa (* k n-tmp))
        (read-sequence a-tmp sa)
        (dotimes (j n-tmp)
          (setf (aref a-tmp j) (funcall f (aref a-tmp j))))
        (seek sa (* k n-tmp))
        (write-sequence a-tmp sa))))
  nil)

(defun disk-pointwise-multiply (fa fb scheme mod-num)
  (let ((na (storage-length fa))
        (nb (storage-length fb)))
    (assert (= na nb))
    (assert (power-of-two-p na))
    (assert (and (not (eq fa fb))
                 (not (eq (file-backed-storage-stream fa)
                          (file-backed-storage-stream fb)))))
    (let* ((sa (file-backed-storage-stream fa))
           (sb (file-backed-storage-stream fb))
           (m (aref (scheme-moduli scheme) mod-num))
           (n-tmp (min na (expt 2 8)))
           (a-tmp (raw-storage-of-storage (make-storage n-tmp)))
           (b-tmp (raw-storage-of-storage (make-storage n-tmp))))
      ;; TODO: no need to re-seek all the time. just read linearly.
      (dotimes (k (floor na n-tmp))
        (seek sa (* k n-tmp))
        (seek sb (* k n-tmp))
        (read-sequence a-tmp sa)
        (read-sequence b-tmp sb)
        (dotimes (j n-tmp)
          (setf (aref a-tmp j) (m* (aref a-tmp j) (aref b-tmp j) m)))
        (seek sa (* k n-tmp))
        (write-sequence a-tmp sa))))
  nil)

(defun disk-convolution (fa fb scheme mod-num)
  (assert (= (storage-length fa)
             (storage-length fb)))
  (when *verbose*
    (write-char #\f))
  (rewind-file-backed-storage fa)
  (rewind-file-backed-storage fb)
  (disk-forward-mfa fa scheme mod-num)
  (disk-forward-mfa fb scheme mod-num)

  (when *verbose*
    (write-char #\p))
  (rewind-file-backed-storage fa)
  (rewind-file-backed-storage fb)
  (disk-pointwise-multiply fa fb scheme mod-num)

  (when *verbose*
    (write-char #\b))
  (rewind-file-backed-storage fa)
  (disk-reverse-mfa fa scheme mod-num)
  (when *verbose*
    (write-char #\Space))
  (rewind-file-backed-storage fa)
  nil)

(defun make-fbs-ntt-work (fx transform-length scheme)
  (loop :for m :across (scheme-moduli scheme)
        :collect (let ((f (copy-storage fx)))
                   (map-into-disk! (lambda (d) (mod d m)) f)
                   (resize-storage f transform-length)
                   (rewind-file-backed-storage f)
                   f)))

(defun fbs-* (fx fy)
  (let* ((size (+ (storage-length fx) (storage-length fy)))
         (length (least-power-of-two->= size))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (num-moduli (num-moduli-needed-for-bits **scheme** bound-bits))
         (ntts-x (make-fbs-ntt-work fx length **scheme**))
         (ntts-y (make-fbs-ntt-work fy length **scheme**))
         (result (first ntts-y))
         (beginning (get-internal-real-time))
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (when *verbose*
                            (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                            (setf start-time (get-internal-real-time))
                            (finish-output))))))
    (when *verbose*
      (format t "~&   Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
              size
              (round (* size $digit-bits)
                     (log 10.0d0 2.00))
              (round (/ (* size $digit-bits) 8 1024 1024)))
      (format t "   Transform length: ~D~%" length)
      (format t "   Convolution bits: ~D~%" bound-bits)
      (format t "   Moduli: ~{#x~16X~^, ~}~%" (coerce (scheme-moduli **scheme**) 'list))
      (format t "   Convolving..."))
    (with-parallel-work ()
     (loop :for i :below num-moduli
           :for ax :in ntts-x
           :for ay :in ntts-y
           :do (with-task (i ax)
                 (disk-convolution ax ay **scheme** i))))
    (funcall report-time)

    ;; Tell the garbage collector we don't need no vectors anymore.
    (setf ntts-y nil)
    (rewind-file-backed-storage result)
    (map-into-disk! (constantly 0) result)
    (rewind-file-backed-storage result)

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
              :sum (* f (storage-ref a i)) :into result-digit
              :finally (add-big-digit/disk (mod result-digit composite) result i)))
      (funcall report-time))
    (rewind-file-backed-storage result)
    (setf result (optimize-storage result))
    (when *verbose*
      (format t "~&Mult time (len ~D): ~D ms~%"
              (storage-length result)
              (round (* 1000 (- (get-internal-real-time) beginning)) internal-time-units-per-second)))
    result))

(defun add-big-digit/disk (big-digit storage i)
  (declare (type unsigned-byte big-digit)
           (type file-backed-storage storage)
           (type alexandria:array-index i)
           (optimize speed))
  ;;(format t "adding ~D to ~A~%" big-digit storage)
  (cond
    ((zerop big-digit) storage)
    ((>= i (storage-length storage))
     (error "Trying to add ~D at index ~D. ~
             This is unexpected and indicates ~
             a grave inconsistency."
            big-digit i))
    ((typep big-digit 'fixnum)
     (multiple-value-bind (x carry) (add64 (storage-ref storage i) big-digit)
       (declare (type bit carry))
       ;;(format t "~&writing ~D~%" x)
       (setf (storage-ref storage i) x)
       (add-big-digit/disk carry storage (1+ i))))
    ;; Specially written to not cons.
    #+sbcl
    (t (dotimes (j (sb-bignum:%bignum-length big-digit) storage)
         (let* ((offset (+ i j))
                (si (storage-ref storage offset))
                (bi (sb-bignum:%bignum-ref big-digit j)))
           (declare (type alexandria:array-index offset)
                    (type (unsigned-byte 64) si bi))
           (multiple-value-bind (x carry) (add64 si bi)
             (setf (storage-ref storage offset) x)
             (add-big-digit/disk carry storage (1+ offset))))))
    ;; Otherwise, we gotta cons...
    #-sbcl
    (t (let ((si (storage-ref storage i))
             (digit-lo64 (ldb (byte $digit-bits 0) big-digit)))
         (declare (type (unsigned-byte 64) si digit-lo64))
         (multiple-value-bind (x carry) (add64 si digit-lo64)
           (declare (type bit carry))
           (setf (storage-ref storage i) x)
           (let ((quo (ash big-digit #.(- $digit-bits))))
             ;; Do QUO + CARRY in two separate steps to avoid a bignum
             ;; addition on the Lisp side.
             (add-big-digit/disk quo storage (1+ i))
             (add-big-digit/disk carry storage (1+ i))))))))


;;; Scratch Work

#+#:ignore
(defun forward-mfa (x scheme mod-num)
  (check-type x raw-storage)
  (assert (power-of-two-p (length x)))
  (let* ((m (aref (scheme-moduli scheme) mod-num))
         (lgn (lg (length x)))
         (w (aref (scheme-primitive-roots scheme) lgn mod-num 0 0))
         ;; factorize
         (rows (expt 2 (floor lgn 2)))
         (cols (expt 2 (- lgn (floor lgn 2))))
         ;; workspace
         (col-work (raw-storage-of-storage (make-storage rows)))
         (row-work (raw-storage-of-storage (make-storage cols))))
    (assert (= (length x) (* rows cols)))
    (format t "MFA: m=#x~X, w=~D, RxC = ~Dx~D~%" m w rows cols)
    (labels ((read-column! (c)
               (dotimes (r rows)
                 (setf (aref col-work r) (aref x (+ c (* r cols))))))
             (write-column! (c)
               (dotimes (r rows)
                 (setf (aref x (+ c (* r cols))) (aref col-work r))))
             (read-row! (r)
               (dotimes (c cols)
                 (setf (aref row-work c) (aref x (+ c (* r cols))))))
             (write-row! (r)
               (dotimes (c cols)
                 (setf (aref x (+ c (* r cols))) (aref row-work c)))))
      ;; Apply a length R xform on each column
      (dotimes (c cols)
        (read-column! c)
        (map-into col-work (lambda (x) (mod x m)) col-work)
        (ntt-forward col-work scheme mod-num)
        (bit-reversed-permute! col-work)
        (write-column! c))
      ;; Multiply twiddle factors
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((w^rc (expt-mod/safe w (* r c) m))
                (rc (+ c (* r cols))))
            (setf (aref x rc) (m* w^rc (aref x rc) m)))))
      ;; Apply a length C xform to each row
      (dotimes (r rows)
        (read-row! r)
        (ntt-forward row-work scheme mod-num)
        (bit-reversed-permute! row-work)
        (write-row! r))))
  x)
