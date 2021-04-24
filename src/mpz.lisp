;;;; mpz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(deftype storage ()
  `(or ram-vec disk-vec))

(defun make-storage (n)
  (check-type n alexandria:array-length)
  (cond
    ((<= 0 n (floor (* 8 *maximum-vector-size*) $digit-bits))
     (make-ram-vec n))
    (t
     (make-disk-vec n))))

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

(defmethod vec-digit-length ((mpz mpz))
  (vec-digit-length (storage mpz)))

(defmethod vec-digit-pointer ((mpz mpz))
  (vec-digit-pointer (storage mpz)))

;;; Conversion functions

(defun mpz-integer (mpz)
  (declare (type mpz mpz))
  (with-vec (mpz mpz_)
    (* (sign mpz)
       (loop :for i :below (vec-digit-length mpz)
             :for digit := (mpz_ i)
             :for b := 1 :then (* b $base)
             :sum (* digit b)))))

(defun integer-mpz (n)
  (declare (type integer n))
  (cond
    ((zerop n)
     (make-mpz 1 (make-storage 1)))
    ((= 1 (abs n))
     (make-mpz n (let ((s (make-storage 1)))
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
                            (dotimes (i len (make-mpz sign vec))
                              (setf (vec_ i) (pop digits)))))))))))

;;; Size, Length, etc.

(declaim (ftype (function (mpz) alexandria:array-length) mpz-size))
(defun mpz-size (mpz)
  "How many digits does the MPZ have?

If MPZ is equal to 0, then this is 0."
  (declare (optimize speed (safety 0) (debug 0)))
  (with-vec (mpz mpz_)
    (loop :for i :from (1- (vec-digit-length mpz)) :downto 0
          :unless (zerop (mpz_ i))
            :do (return (1+ i))
          :finally (return 0))))

(defun mpz-uses-minimal-storage-p (mpz)
  (= (vec-digit-length mpz) (mpz-size mpz)))

(defun mpz-set-zero! (mpz)
  (setf (sign mpz) 1)
  ;; TODO: make efficient by resizing
  (with-vec (mpz mpz_)
    (dotimes (i (vec-digit-length mpz))
      (setf (mpz_ i) 0)))
  nil)

(defun mpz-integer-length (mpz)
  "How many bits are needed to represent MPZ in two's complement?"
  (let ((size (mpz-size mpz)))
    (if (zerop size)
        0
        (with-vec (mpz mpz_)
          (+ (* $digit-bits (1- size))
             (integer-length (mpz_ (1- size)))
             (/ (1- (sign mpz)) 2))))))

(defmethod print-object ((mpz mpz) stream)
  (print-unreadable-object (mpz stream :type t :identity t)
    (format stream "~D bit~:P~:[~;*~]"
            (mpz-integer-length mpz)
            (mpz-uses-minimal-storage-p mpz))))


;;; Comparison functions

(defun mpz-zerop (mpz)
  (do-digits (i digit mpz t)
    (declare (ignore i))
    (unless (zerop digit)
      (return-from mpz-zerop nil))))

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
  (with-vecs (a a_ b b_)
    (and (= (sign a) (sign b))
         (= (mpz-size a) (mpz-size b))
         (vec= (storage a) (storage b)))))

(defun mpz-/= (a b)
  (not (mpz-= a b)))

(defun %mpz-> (a b)
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

(defun %mpz-+ (a b)
  (assert (= 1 (sign a) (sign b)))
  (let* ((size-a (mpz-size a))
         (size-b (mpz-size b))
         (r      (make-storage (1+ (max size-a size-b)))))
    (if (>= size-a size-b)
        (make-mpz 1 (%add-storages/unsafe r
                                          (storage a) size-a
                                          (storage b) size-b))
        (make-mpz 1 (%add-storages/unsafe r
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

(defun %mpz-- (a b)
  (assert (= 1 (sign a) (sign b)))
  (cond
    ((mpz-= a b)
     (integer-mpz 0))
    ((mpz-> a b)
     ;; TODO: optimize storage
     (make-mpz 1 (%subtract-storages/unsafe (storage a) (mpz-size a)
                                            (storage b) (mpz-size b))))
    (t
     ;; TODO: optimize storage
     (make-mpz -1 (%subtract-storages/unsafe (storage b) (mpz-size b)
                                             (storage a) (mpz-size a))))))

(defun mpz-+ (a b)
  (declare (type mpz a b))
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
     (mpz-set-zero! mpz)
     nil)
    ((= 1 d)
     ;; Do absolutely nothin'!
     nil)
    (t
     (let ((d    (abs d))
           (size (mpz-size mpz))
           (carry 0))
       (declare (type (unsigned-byte 64) d)
                (type alexandria:array-length size)
                (type (unsigned-byte 64) carry))
       (with-vec (mpz mpz_)
         (dotimes (i size)
           (multiple-value-bind (lo hi) (mul128 d (mpz_ i))
             (multiple-value-bind (lo sub-carry) (add64 lo carry)
               (setf (mpz_ i) lo)
               (setf carry (fx+ hi sub-carry)))))
         (when (plusp carry)
           (when (mpz-uses-minimal-storage-p mpz)
             (resize-vec-by (storage mpz) 1))
           (setf (mpz_ size) carry)))
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
    (with-vecs (a a_ b b_ r r_)
      (let ((temp (make-mpz 1 (make-storage length))))
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

(defun mpz-debug (mpz &optional (stream *standard-output*))
  (print-unreadable-object (mpz stream :type t :identity nil)
    (format stream "(~D) ~:[-1~;+1~] *"
            (mpz-size mpz)
            (= 1 (sign mpz)))
    (do-digits (i digit mpz)
      (declare (ignore i))
      (format stream " ~16,'0X" digit))
    (terpri stream)
    (format stream "~D" (mpz-integer mpz))))
