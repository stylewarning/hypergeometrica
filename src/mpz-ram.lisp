;;;; mpz-ram.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

;;; This file contains an in-memory implementation of integers.

(deftype storage ()
  '(or ram-vec disk-vec))

(defun make-storage (n)
  (cond
    ((>= *maximum-vector-size* (ceiling (* n $digit-bits) 8))
     (make-ram-vec n))
    (t
     (make-disk-vec n))))

(defclass mpz/ram ()
  ((sign :initarg :sign
         :accessor sign)
   (storage :initarg :storage
            :reader storage))
  (:documentation "An arbitrary precision integer that fits in RAM."))


;;; Conveniences to access the data. This allows us to use WITH-VEC.

(defmethod vec-digit-length ((mpz mpz/ram))
  (vec-digit-length (storage mpz)))

(defmethod vec-digit-pointer ((mpz mpz/ram))
  (vec-digit-pointer (storage mpz)))


;;; Protocol methods.

(defmethod mpz-integer ((mpz mpz/ram))
  (with-vec (mpz mpz_)
    (* (sign mpz)
       (loop :for i :below (vec-digit-length mpz)
             :for digit := (mpz_ i)
             :for b := 1 :then (* b $base)
             :sum (* digit b)))))

(defmethod integer-mpz ((n integer) (class-name (eql 'mpz/ram)))
  (declare (ignore class-name))
  (cond
    ((zerop n)
     (make-instance 'mpz/ram
       :sign 1
       :storage (make-storage 1)))
    ((= 1 (abs n))
     (make-instance 'mpz/ram
       :sign n
       :storage (let ((s (make-storage 1)))
                  (with-vec (s s_)
                    (setf (s_ 0) 1))
                  s)))
    (t
     (let ((sign (signum n)))
       (setf n (abs n))
       (loop :until (zerop n)
             :collect (multiple-value-bind (quo rem) (floor n $base)
                        (setf n quo)
                        rem)
               :into digits
             :finally (return
                        (let ((len (length digits))
                              (vec (make-storage (length digits))))
                          (with-vec (vec vec_)
                            (dotimes (i len (make-instance 'mpz/ram
                                              :sign sign
                                              :storage vec))
                              (setf (vec_ i) (pop digits)))))))))))

(defmethod mpz-size ((mpz mpz/ram))
  (vec-digit-length* (storage mpz)))

(defmethod mpz-uses-minimal-storage-p (mpz)
  (= (vec-digit-length mpz) (mpz-size mpz)))

(defmethod optimize-mpz ((mpz mpz/ram))
  (optimize-storage (storage mpz))
  mpz)

(defun optimize-storage (vec)
  (declare (type storage vec))
  (let ((size (vec-digit-length* vec))
        (length (vec-digit-length vec)))
    (resize-vec-by vec (- size length))
    vec))

(defmethod mpz-set-zero! ((mpz mpz/ram))
  (setf (sign mpz) 1)
  (resize-vec-by (storage mpz) (- (vec-digit-length mpz)))
  nil)

(defmethod mpz-bit-size ((mpz mpz/ram))
    (let ((size (mpz-size mpz)))
    (if (zerop size)
        0
        (with-vec (mpz mpz_)
          (+ (* $digit-bits (1- size))
             (integer-length (mpz_ (1- size)))
             (/ (1- (sign mpz)) 2))))))

(defmethod print-object ((mpz mpz/ram) stream)
  (print-unreadable-object (mpz stream :type t :identity t)
    (format stream "~D bit~:P~:[~;*~]"
            (mpz-integer-length mpz)
            (mpz-uses-minimal-storage-p mpz))))


;;; Comparison functions.

(defmethod mpz-zerop ((mpz mpz/ram))
  (do-digits (i digit mpz t)
    (declare (ignore i))
    (unless (zerop digit)
      (return-from mpz-zerop nil))))

(defmethod mpz-plusp ((mpz mpz/ram))
  (and (= 1 (sign mpz))
       (not (mpz-zerop mpz))))

(defmethod mpz-minusp ((mpz mpz/ram))
  (and (= -1 (sign mpz))
       (not (mpz-zerop mpz))))

(defmethod mpz-abs ((mpz mpz/ram))
  (make-instance 'mpz/ram
    :sign 1
    :storage (storage mpz)))

(defmethod mpz-negate ((mpz mpz/ram))
  (make-instance 'mpz/ram
    :sign (- (sign mpz))
    :storage (storage mpz)))

(defmethod mpz-negate! ((mpz mpz/ram))
  (setf (sign mpz) (- (sign mpz)))
  nil)

(defmethod mpz-= ((a mpz/ram) (b mpz/ram))
  (with-vecs (a a_ b b_)
    (and (= (sign a) (sign b))
         (= (mpz-size a) (mpz-size b))
         (vec= (storage a) (storage b)))))

(defun %mpz/ram-> (a b)
  (assert (= 1 (sign a) (sign b)))
  (let ((size-a (mpz-size a))
        (size-b (mpz-size b)))
    (if (/= size-a size-b)
        (> size-a size-b)
        (with-vecs (a a_ b b_)
          (loop :for i :from (1- size-a) :downto 0
                :for ai := (a_ i)
                :for bi := (b_ i)
                :do (cond
                      ((> ai bi) (return t))
                      ((< ai bi) (return nil)))
                :finally (return nil))))))

(defmethod mpz-> ((a mpz/ram) (b mpz/ram))
  (cond
    ((> (sign a) (sign b)) t)
    ((< (sign a) (sign b)) nil)
    ;; Now we know they have the same sign. If they're both negative,
    ;; then reverse.
    ((= -1 (sign a))
     (%mpz/ram-> (mpz-abs b) (mpz-abs a)))
    ;; They have the same sign and they're positive.
    (t
     (%mpz/ram-> a b))))

(defun %add-storages/unsafe (r a size-a b size-b)
  (declare (type storage r a b)
           (type alexandria:array-length size-a size-b))
  #+hypergeometrica-safe
  (assert (>= size-a size-b))
  #+hypergeometrica-safe
  (assert (>= (vec-digit-length r) size-a))
  ;; size-a >= size-b
  (with-vecs (a a_ b b_ r r_)
    (let ((carry 0))
      (declare (type digit carry))
      (dotimes (i size-b)
        ;; AI + BI + CARRY
        (multiple-value-bind (sum c) (add64 (a_ i) (b_ i))
          ;; XXX: Using MULTIPLE-VALUE-SETQ is buggy here in SBCL
          ;; 2.1.3.
          (multiple-value-bind (sum new-carry) (add64 sum carry)
            (setf carry (+ new-carry c))
            (setf (r_ i) sum))))
      (do-range (i size-b size-a)
        (multiple-value-bind (sum new-carry) (add64 (a_ i) carry)
          (setf carry new-carry
                (r_ i) sum)))
      ;; Account for the carry.
      (unless (zerop carry)
        ;; Do we have enough storage? If not, make some.
        (assert (< size-a (vec-digit-length r)))
        (setf (r_ size-a) carry))
      ;; Return the storage
      r)))

(defun %mpz/ram-+ (a b)
  (assert (= 1 (sign a) (sign b)))
  (let* ((size-a (mpz-size a))
         (size-b (mpz-size b))
         (r      (make-storage (1+ (max size-a size-b)))))
    (if (>= size-a size-b)
        (make-instance 'mpz/ram
          :sign 1
          :storage (%add-storages/unsafe r
                                         (storage a) size-a
                                         (storage b) size-b))
        (make-instance 'mpz/ram
          :sign 1
          :storage (%add-storages/unsafe r
                                         (storage b) size-b
                                         (storage a) size-a)))))

(defun %subtract-storages/unsafe (a size-a b size-b)
  (declare (type storage a b)
           (type alexandria:array-length size-a size-b))
  ;; a > b > 0
  (assert (>= size-a size-b))

  (let* ((r (make-storage size-a))
         (carry 1))
    (declare (type bit carry))
    (with-vecs (a a_ b b_ r r_)
      (dotimes (i size-b)
        (let ((ai (a_ i))
              (neg-bi (complement-digit (b_ i))))
          ;; AI + CARRY - BI
          (multiple-value-bind (sum c) (add64 ai carry)
            (cond
              ((= 1 c)
               (setf (r_ i) neg-bi
                     carry        1))
              (t
               ;; XXX: Using MULTIPLE-VALUE-SETQ is buggy here in SBCL
               ;; 2.1.3.
               (multiple-value-bind (sum new-carry) (add64 sum neg-bi)
                 (setf carry new-carry)
                 (setf (r_ i) sum)))))))
      (do-range (i size-b size-a)
        (let ((ai (a_ i)))
          (multiple-value-bind (sum c) (add64 ai carry)
            (cond
              ((= 1 c)
               (setf (r_ i) $max-digit
                     carry        1))
              (t
               ;; XXX: Using MULTIPLE-VALUE-SETQ is buggy here in SBCL
               ;; 2.1.3.
               (multiple-value-bind (sum new-carry) (add64 sum $max-digit)
                 (setf carry new-carry)
                 (setf (r_ i) sum))))))))
    #+hypergeometrica-safe
    (assert (= 1 carry))
    r))

(defun %mpz/ram-- (a b)
  (assert (= 1 (sign a) (sign b)))
  (cond
    ((mpz-= a b)
     (integer-mpz 0 'mpz/ram))
    ((mpz-> a b)
     (make-instance 'mpz/ram
       :sign 1
       :storage (optimize-storage
                 (%subtract-storages/unsafe (storage a) (mpz-size a)
                                            (storage b) (mpz-size b)))))
    (t
     (make-instance 'mpz/ram
       :sign -1
       :storage (optimize-storage
                 (%subtract-storages/unsafe (storage b) (mpz-size b)
                                            (storage a) (mpz-size a)))))))

(defmethod mpz-+ ((a mpz/ram) (b mpz/ram))
  (cond
    ((mpz-zerop a) b)
    ((mpz-zerop b) a)
    ((= 1 (sign a) (sign b))
     (%mpz/ram-+ a b))
    ((= -1 (sign a) (sign b))
     (mpz-negate (%mpz/ram-+ (mpz-abs a) (mpz-abs b))))
    ;; a > 0, b < 0
    ((= -1 (sign b))
     (%mpz/ram-- a (mpz-abs b)))
    ;; a < 0, b > 0
    (t
     (%mpz/ram-- b (mpz-abs a)))))

(defmethod mpz-- ((a mpz/ram) (b mpz/ram))
  (mpz-+ a (mpz-negate b)))

(defmethod mpz-left-shift ((a mpz/ram) (n integer))
  (cond
    ((zerop n)
     a)
    (t
     (multiple-value-bind (quo rem)
         (floor n $digit-bits)
       (cond
         ((zerop rem)
          (let* ((size-a (mpz-size a))
                 (storage-r (make-storage (+ quo size-a))))
            (with-vecs (a a_ storage-r r_)
              ;; Copy the bits
              (loop :for i :below size-a
                    :for j :from quo
                    :do (setf (r_ j) (a_ i))))
            (make-instance 'mpz/ram
              :sign (sign a)
              :storage storage-r)))
         (t
          (error "not implemented")))))))

(defmethod mpz-multiply-by-digit! (d (mpz mpz/ram))
  (declare (type digit d))
  (cond
    ((zerop d)
     (mpz-set-zero! mpz)
     nil)
    ((= 1 d)
     ;; Do absolutely nothin'!
     nil)
    (t
     (let ((d    (abs d))
           (size (mpz-size mpz))
           (carry 0)
           (optimized-storage? (mpz-uses-minimal-storage-p mpz)))
       (declare (type (unsigned-byte 64) d)
                (type alexandria:array-length size)
                (type (unsigned-byte 64) carry))
       (with-vec (mpz mpz_)
         (dotimes (i size)
           (multiple-value-bind (lo hi) (mul128 d (mpz_ i))
             (multiple-value-bind (lo sub-carry) (add64 lo carry)
               (setf (mpz_ i) lo)
               (setf carry (fx+ hi sub-carry))))))
       (when (plusp carry)
         (when optimized-storage?
           (resize-vec-by (storage mpz) 1))
         (with-vec (mpz mpz_)
           (setf (mpz_ size) carry)))
       nil))))

(defmethod mpz-multiply-by-s64! (d (mpz mpz/ram))
  (declare (type (signed-byte 64) d))
  (mpz-multiply-by-digit! (abs d) mpz)
  (when (minusp d)
    (mpz-negate! mpz))
  nil)

(defun %multiply-storage/schoolboy (a a-size b b-size)
  #+hypergeometrica-safe
  (assert (>= a-size b-size))
  (let* ((length (+ 1 a-size b-size))
         (r (make-storage length)))
    (with-vecs (a a_ b b_ r r_)
      (let ((temp (make-instance 'mpz/ram
                    :sign 1
                    :storage (make-storage length))))
        (dotimes (i b-size r)
          (let ((bi (b_ i)))
            (unless (zerop bi)
              ;; We re-use TEMP. Clear dirty info. (XXX: We could
              ;; probably optimize START and END.)
              ;;
              ;; TEMP := 0
              (vec-fill temp 0)
              ;; :START1 i should be interpreted as a left shift of A by I
              ;; digits.
              ;;
              ;; TEMP := A * 2^(64 * I).
              (vec-replace/unsafe temp a :start1 i)
              ;; TEMP := B[i] * TEMP
              (mpz-multiply-by-digit! bi temp)
              ;; R := R + TEMP
              (%add-storages/unsafe r
                                    r              length
                                    (storage temp) length))))))))

;;;

(defmethod mpz-debug ((mpz mpz/ram) &optional (stream *standard-output*))
  (print-unreadable-object (mpz stream :type t :identity nil)
    (format stream "(~D) ~:[-1~;+1~] *"
            (mpz-size mpz)
            (= 1 (sign mpz)))
    (do-digits (i digit mpz)
      (declare (ignore i))
      (format stream " ~16,'0X" digit))
    (terpri stream)
    (format stream "~D" (mpz-integer mpz))))
