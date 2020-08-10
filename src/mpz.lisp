;;;; mpz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype sign ()
  "The sign of an integer."
  '(member 1 -1))

(defstruct (mpz (:conc-name nil)
                (:predicate mpz?)
                (:copier nil)
                (:constructor make-mpz (sign storage)))
  (sign 1 :type sign)
  (storage (make-storage 0) :type storage :read-only t))
#+sbcl (declaim (sb-ext:freeze-type mpz))

(declaim (ftype (function (mpz) raw-storage) raw-storage))
(defun raw-storage (mpz)
  (raw-storage-of-storage (storage mpz)))

;;; Conversion functions

(defun mpz-integer (mpz)
  (declare (type mpz mpz))
  (* (sign mpz)
     (loop :for digit :across (storage mpz)
           :for b := 1 :then (* b $base)
           :sum (* digit b))))

(defun integer-mpz (n)
  (declare (type integer n))
  (cond
    ((zerop n)
     (make-mpz 1 (make-storage 1)))
    ((= 1 n)
     (make-mpz 1 (let ((s (make-storage 1)))
                   (setf (aref s 0) 1)
                   s)))
    (t
     (let ((sign (signum n)))
       (setf n (abs n))
       (loop :until (zerop n)
             :collect (multiple-value-bind (quo rem) (floor n $base)
                        (setf n quo)
                        rem)
               :into digits
             :finally (return (let ((storage (make-storage (length digits))))
                                (replace storage digits)
                                (make-mpz sign storage))))))))

;;; Size, Length, etc.

(declaim (ftype (function (mpz) alexandria:array-length) mpz-size))
(defun mpz-size (mpz)
  "How many digits does the MPZ have?

If MPZ is equal to 0, then this is 0."
  (declare (optimize speed (safety 0) (debug 0)))
  (loop :with raw := (raw-storage mpz)
        :for i :from (1- (length raw)) :downto 0
        :unless (zerop (aref raw i))
          :do (return (1+ i))
        :finally (return 0)))

(defun mpz-uses-minimal-storage-p (mpz)
  (= (length (storage mpz)) (mpz-size mpz)))

(defmethod optimize-storage ((mpz mpz))
  (resize-storage (storage mpz) (mpz-size mpz))
  mpz)

(defun mpz-set-zero! (mpz)
  (setf (sign mpz) 1)
  (resize-storage (storage mpz) 0)
  nil)

(defun mpz-integer-length (mpz)
  "How many bits are needed to represent MPZ in two's complement?"
  (let ((size (mpz-size mpz)))
    (if (zerop size)
        0
        (+ (* $digit-bits (1- size))
           (integer-length (aref (storage mpz) (1- size)))
           (/ (1- (sign mpz)) 2)))))

(defmethod print-object ((mpz mpz) stream)
  (print-unreadable-object (mpz stream :type t :identity t)
    (format stream "~D bit~:P~:[~;*~]"
            (mpz-integer-length mpz)
            (mpz-uses-minimal-storage-p mpz))))


;;; Comparison functions

(defun mpz-zerop (mpz)
  (every #'zerop (raw-storage mpz)))

(defun mpz-plusp (mpz)
  (and (= 1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-minusp (mpz)
  (and (= -1 (sign mpz))
       (not (mpz-zerop mpz))))

(defun mpz-abs (mpz)
  (make-mpz 1 (storage mpz)))

(defun mpz-negate (mpz)
  (make-mpz (- (sign mpz)) (storage mpz)))

(defun mpz-negate! (mpz)
  (setf (sign mpz) (- (sign mpz)))
  nil)

(defun mpz-= (a b)
  (and (= (sign a) (sign b))
       (= (mpz-size a) (mpz-size b))
       (every #'= (storage a) (storage b))))

(defun mpz-/= (a b)
  (not (mpz-= a b)))

(defun %mpz-> (a b)
  (assert (= 1 (sign a) (sign b)))
  (let ((size-a (mpz-size a))
        (size-b (mpz-size b)))
    (if (/= size-a size-b)
        (> size-a size-b)
        (loop :for i :from (1- size-a) :downto 0
              :for ai := (aref (storage a) i)
              :for bi := (aref (storage b) i)
              :do (cond
                    ((> ai bi) (return t))
                    ((< ai bi) (return nil)))
              :finally (return nil)))))

(defun mpz-> (a b)
  (cond
    ((> (sign a) (sign b)) t)
    ((< (sign a) (sign b)) nil)
    ;; Now we know they have the same sign. If they're both negative,
    ;; then reverse.
    ((= -1 (sign a))
     (%mpz-> (mpz-abs b) (mpz-abs a)))
    ;; They have the same sign and they're positive.
    (t
     (%mpz-> a b))))

(defun mpz->= (a b)
  (or (mpz-= a b)
      (mpz-> a b)))

(defun mpz-< (a b)
  (not (mpz->= a b)))

(defun mpz-<= (a b)
  (not (mpz-> a b)))

(defun %add-storages/unsafe (r a size-a b size-b)
  (declare (type storage r)
           (type raw-storage a b)
           (type alexandria:array-length size-a size-b))
  #+hypergeometrica-safe
  (assert (>= size-a size-b))
  #+hypergeometrica-safe
  (assert (>= (length r) size-a))
  ;; size-a >= size-b
  (let* ((raw   (raw-storage-of-storage r))
         (carry 0))
    (declare (type digit carry))
    (dotimes (i size-b)
      (let ((ai (aref a i))
            (bi (aref b i)))
        ;; AI + BI + CARRY
        (multiple-value-bind (sum c) (add64 ai bi)
          (multiple-value-setq (sum carry) (add64 sum carry))
          (incf carry c)
          (setf (aref raw i) sum))))
    (do-range (i size-b size-a)
      (let ((ai (aref a i)))
        (multiple-value-bind (sum new-carry) (add64 ai carry)
          (setf carry new-carry
                (aref raw i) sum))))
    ;; Account for the carry.
    (unless (zerop carry)
      ;; Do we have enough storage? If not, make some.
      (unless (< size-a (length r))
        (resize-storage-by r 1))
      (setf (aref raw size-a) carry))
    ;; Return the storage
    r))

(defun %mpz-+ (a b)
  (assert (= 1 (sign a) (sign b)))
  (let* ((size-a (mpz-size a))
         (size-b (mpz-size b))
         (r      (make-storage (1+ (max size-a size-b)))))
    (if (>= size-a size-b)
        (make-mpz 1 (%add-storages/unsafe r
                                          (raw-storage a) size-a
                                          (raw-storage b) size-b))
        (make-mpz 1 (%add-storages/unsafe r
                                          (raw-storage b) size-b
                                          (raw-storage a) size-a)))))

(defun %subtract-storages/unsafe (a size-a b size-b)
  (declare (type raw-storage a b)
           (type alexandria:array-length size-a size-b))
  ;; a > b > 0
  (assert (>= size-a size-b))
  (let* ((r (make-storage size-a))
         (raw (raw-storage-of-storage r))
         (carry 1))
    (declare (type bit carry))
    (dotimes (i size-b)
      (let ((ai (aref a i))
            (neg-bi (complement-digit (aref b i))))
        ;; AI + CARRY - BI
        (multiple-value-bind (sum c) (add64 ai carry)
          (cond
            ((= 1 c)
             (setf (aref raw i) neg-bi
                   carry        1))
            (t
             (multiple-value-setq (sum carry) (add64 sum neg-bi))
             (setf (aref raw i) sum))))))
    (do-range (i size-b size-a)
      (let ((ai (aref a i)))
        (multiple-value-bind (sum c) (add64 ai carry)
          (cond
            ((= 1 c)
             (setf (aref raw i) $max-digit
                   carry        1))
            (t
             (multiple-value-setq (sum carry) (add64 sum $max-digit))
             (setf (aref raw i) sum))))))
    #+hypergeometrica-safe
    (assert (= 1 carry))
    r))

(defun %mpz-- (a b)
  (assert (= 1 (sign a) (sign b)))
  (cond
    ((mpz-= a b)
     (integer-mpz 0))
    ((mpz-> a b)
     (optimize-storage
      (make-mpz 1 (%subtract-storages/unsafe (raw-storage a) (mpz-size a)
                                             (raw-storage b) (mpz-size b)))))
    (t
     (optimize-storage
      (make-mpz -1 (%subtract-storages/unsafe (raw-storage b) (mpz-size b)
                                              (raw-storage a) (mpz-size a)))))))

(defun mpz-+ (a b)
  (cond
    ((mpz-zerop a) b)
    ((mpz-zerop b) a)
    ((= 1 (sign a) (sign b))
     (%mpz-+ a b))
    ((= -1 (sign a) (sign b))
     (mpz-negate (%mpz-+ (mpz-abs a) (mpz-abs b))))
    ;; a > 0, b < 0
    ((= -1 (sign b))
     (%mpz-- a (mpz-abs b)))
    ;; a < 0, b > 0
    (t
     (%mpz-- b (mpz-abs a)))))

(defun mpz-- (a b)
  (mpz-+ a (mpz-negate b)))

(defun mpz-multiply-by-digit! (d mpz)
  (declare (type digit d)
           (type mpz mpz))
  (cond
    ((zerop d)
     (setf (sign mpz) 1)
     (resize-storage (storage mpz) 0)
     nil)
    ((= 1 d)
     ;; Do absolutely nothin!
     nil)
    (t
     (let ((d    (abs d))
           (size (mpz-size mpz))
           (carry 0))
       (declare (type (unsigned-byte 64) d)
                (type alexandria:array-length size)
                (type (unsigned-byte 64) carry))
       (let ((raw (raw-storage mpz)))
         (declare (type raw-storage raw))
         (dotimes (i size)
           (multiple-value-bind (lo hi) (mul128 d (aref raw i))
             (multiple-value-bind (lo sub-carry) (add64 lo carry)
               (setf (aref raw i) lo)
               (setf carry (fx+ hi sub-carry))))))
       (when (plusp carry)
         (when (mpz-uses-minimal-storage-p mpz)
           (resize-storage-by (storage mpz) 1))
         (setf (aref (raw-storage mpz) size) carry))
       nil))))

(defun mpz-multiply-by-s64! (d mpz)
  (declare (type (signed-byte 64) d)
           (type mpz mpz))
  (mpz-multiply-by-digit! (abs d) mpz)
  (when (minusp d)
    (mpz-negate! mpz))
  nil)

(defun %multiply-storage/schoolboy (a a-size b b-size)
  #+hypergeometrica-safe
  (assert (>= a-size b-size))
  (let* ((length (+ 1 a-size b-size))
         (r (make-storage length)))
    (let ((temp (make-mpz 1 (make-storage length))))
      (dotimes (i b-size r)
        (let ((bi (aref b i)))
          (unless (zerop bi)
            ;; We re-use TEMP. Clear dirty info. (XXX: We could
            ;; probably optimize START and END.)
            ;;
            ;; TEMP := 0
            (fill (raw-storage temp) 0)
            ;; :START1 i should be interpreted as a left shift of A by I
            ;; digits.
            ;;
            ;; TEMP := A * 2^(64 * I).
            (replace (raw-storage temp) a :start1 i)
            ;; TEMP := B[i] * TEMP
            (mpz-multiply-by-digit! bi temp)
            ;; R := R + TEMP
            (%add-storages/unsafe r
                                  (raw-storage-of-storage r) length
                                  (raw-storage temp)         length)))))))

;;;

(defun mpz-debug (mpz &optional (stream *standard-output*))
  (print-unreadable-object (mpz stream :type t :identity nil)
    (format stream "(~D) ~:[-1~;+1~] *"
            (mpz-size mpz)
            (= 1 (sign mpz)))
    (dotimes (i (mpz-size mpz))
      (format stream " ~16,'0X" (aref (storage mpz) i)))
    (terpri stream)
    (format stream "~D" (mpz-integer mpz))))
