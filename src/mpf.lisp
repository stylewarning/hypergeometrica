;;;; mpf.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

;;; Float Representation

(defconstant $min-exponent-value most-negative-fixnum)
(defconstant $max-exponent-value most-positive-fixnum)

(defconstant $nul-exponent-value $min-exponent-value)
(defconstant $inf-exponent-value (1- $max-exponent-value))
(defconstant $nan-exponent-value $max-exponent-value)

(defconstant $min-exponent (+  1 $min-exponent-value)) ; Maybe reserve another bit?
(defconstant $max-exponent (+ -2 $max-exponent-value)) ; Maybe reserve another bit?

;;; This is a representation of a Multiple-Precision Float (MPF). Aside from representing a fractional value, it may also represent:
;;;
;;;     - NaN,
;;;
;;;     - Positive or negative infinity.
;;;
;;;     - Positive or negative zero.
;;;
;;; SIGN is either +1 or -1
;;;
;;; EXPONENT is a fixnum, but its meaning is overloaded in the above
;;; ways. We use *-VALUE to denote overloaded (i.e., non-literal)
;;; meaning.
;;;
;;; STORAGE is a VEC of DIGITs.
;;;
;;; Let S be the sign, E be the exponent, and D be the vector of N
;;; digits. Also let 2^B be the base (i.e., B = $DIGIT-BITS). Then the
;;; value of an MPF is
;;;
;;; Sign           Mantissa
;;; |          [<----------->]
;;; v           N - 1
;;;             =====
;;;    E - B N  \          B i
;;; S 2          >     D  2
;;;             /       i
;;;             =====
;;;             i = 0
;;;   [<---->]
;;;     Bias
;;;
;;; This means STORAGE stores the digits in LSB-to-MSB order.
;;;
;;; See MPF-RATIONAL for a computable version of this definition.
(defclass mpf ()
  ((sign :initarg :sign :accessor mpf-sign)
   (exponent :initarg :exponent :accessor mpf-exponent)
   (storage :initarg :storage :accessor mpf-storage)))

(defun mpf? (x)
  (typep x 'mpf))

(defun make-mpf (sign exponent storage)
  (declare (type sign sign)
           (type fixnum exponent)
           (type storage storage))
  (make-instance 'mpf :sign sign :exponent exponent :storage storage))

(defun mpf-finite? (x)
  (< (mpf-exponent x) $inf-exponent-value))

(defun mpf-infinite? (x)
  (not (mpf-finite? x)))

(defun mpf-nan? (x)
  (= (mpf-exponent x) $nan-exponent-value))

(defun mpf-zero? (x)
  (= (mpf-exponent x) $nul-exponent-value))

(defun mpf-integer? (x)
  (and (mpf-finite? x)
       (not (minusp (mpf-smallest-power x)))))

(defun mpf-smallest-power (x)
  ;; What is the power of the least-significant 1-bit?
  ;;
  ;; See also: MPF-BIAS.
  (assert (mpf-finite? x))
  (let* ((s (mpf-storage x))
         (n (vec-digit-length s)))
    (cond
      ((zerop n)
       0)
      (t
       (with-vec (s s_)
         ;; If X is normalized, we wouldn't have to loop. We'd be
         ;; guaranteed that the first digit is non-zero.
         (loop :for i :below n
               ;; Ignoring the exponent, this digit represents 2^(N*B)
               ;; to 2^((N-1)B+1). We have to find the exponent in
               ;; that range.
               :for bit := (* n $digit-bits) :then (- bit $digit-bits)
               :for d := (s_ i)
               :unless (zerop d)
                 :do (return
                       ;; Find the bit that's set and shift by our
                       ;; exponent.
                       (- (mpf-exponent x)
                          (- bit (ctz d))))))))))


(defun mpf-mantissa (x)
  #+hypergeometrica-paranoid
  (assert (mpf-finite? x))
  (let ((mantissa 0))    ;; This is a wasteful bit simple-to-understand computation.
    (do-digits (i d (mpf-storage x) mantissa)
      (incf mantissa (* d (expt 2 (* i $digit-bits)))))))

(defun mpf-bias (x)
  #+hypergeometrica-paranoid
  (assert (mpf-finite? x))
  (expt 2 (- (mpf-exponent x)
             (* $digit-bits (vec-digit-length (mpf-storage x))))))

(defun mpf-rational (x)
  ;; This function serves to *define* what an MPF is.
  (cond
    ((mpf-nan? x) ':nan)
    ((mpf-infinite? x) (if (plusp (mpf-sign x))
                           ':positive-infinity
                           ':negative-infinity))
    ((mpf-zero? x)
     0)
    (t
     ;; This is a wasteful but simple-to-understand computation.
     (* (mpf-sign x) (mpf-bias x) (mpf-mantissa x)))))

(defmethod print-object ((x mpf) stream)
  (print-unreadable-object (x stream :type t :identity nil)
    (cond
      ((mpf-nan? x)      (write-string "NaN" stream))
      ((mpf-infinite? x) (format stream "~:[-~;+~]Inf" (= 1 (mpf-sign x))))
      ((mpf-zero? x)     (format stream "~:[-~;+~]0" (= 1 (mpf-sign x))))
      (t
       (let* ((num-digits  (vec-digit-length (mpf-storage x)))
              (first-digit (vec-ref (mpf-storage x) (1- num-digits))))
         (format stream "#x~:[-~;+~]0.~v,'0X~:[...~;~]`~D (~D digit~:P)"
                 (= 1 (mpf-sign x))
                 (/ $digit-bits 2 4)
                 (ash first-digit (- (/ $digit-bits 2)))
                 (and (= 1 num-digits)
                      (zerop (ldb (byte (/ $digit-bits 2) 0) first-digit)))
                 (mpf-exponent x)
                 num-digits))))))

;;; Lower Level Accessors

(defun mpf-digit (x n)
  "Get the Nth \"mathematical digit\" of X."
  (if (<= 0 n (1- (vec-digit-length (mpf-storage x))))
      (vec-ref (mpf-storage x) n)
      0))

(defun mpf-lsbs (x n)
  "Get $DIGIT-BITS least-significant bits starting at bit-position N."
  (multiple-value-bind (digit-num offset) (floor n $digit-bits)
    (if (zerop offset)
        (mpf-digit x digit-num)
        (let ((this-digit (mpf-digit x digit-num))
              (next-digit (mpf-digit x (1+ digit-num))))
          (declare (type digit this-digit next-digit))
          (setf this-digit (ash this-digit (- offset)))      ; size: DIGIT_BITS - offset
          (setf next-digit (ldb (byte offset 0) next-digit)) ; size: offset
          (logior this-digit (ash next-digit (- $digit-bits offset)))))))

(defun mpf-lsb (x n)
  "Get the Nth least-significant \"mathematical bit\" of X."
  (multiple-value-bind (digit-num offset) (floor n $digit-bits)
    (ldb (byte 1 offset) (mpf-digit x digit-num))))

(defun mpf-test-msbs (x n)
  "Return a boolean indicating whether a 1 bit exists in bits from positions 0 to N, where 0 corresponds to the MSB."
  (cond
    ((mpf-zero? x) nil)
    (t
     (with-vec ((mpf-storage x) x_)
       (multiple-value-bind (digit-count slack) (floor n $digit-bits)
         #+hypergeometrica-safe
         (assert (< digit-count (vec-digit-length (mpf-storage x))))
         ;; Do we have partial bits?
         (unless (zerop slack)
           ;; Yes... look in the partial bits...
           (when (ldb-test (byte (1+ slack) 0) (x_ digit-count))
             (return-from mpf-test-msbs t))
           (decf digit-count))
         ;; Look in the rest of the bits...
         (loop :for i :from digit-count :downto 0
                 :thereis (plusp (x_ i))))))))


(defun describe-mpf (x &key (decimal-digits-per-iteration 1))
  (cond
    ((mpf-finite? x)
     (let* ((q (abs (mpf-rational x)))
            (num (numerator q))
            (den (denominator q)))
       (multiple-value-bind (quo rem) (floor num den)
         (when (= -1 (mpf-sign x))
           (write-char #\-))
         (format t "~D" quo)
         (write-char #\.)
         (loop :for i :from 0
               :until (zerop rem) :do
                 (multiple-value-bind (frac rest) (floor (* rem (expt 10 decimal-digits-per-iteration)) den)
                   (format t "~D" frac)
                   (setf rem rest))
               :finally (if (zerop i)
                            (format t " [integer]")
                            (format t " [~D decimal~:P]" (* decimal-digits-per-iteration i)))))))
    (t
     (format t "~A" x))))
