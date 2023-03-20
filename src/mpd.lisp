;;;; mpd.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

(defclass mpd ()
  ((mantissa :initarg :mantissa
             :accessor mantissa)
   (exponent :initarg :exponent
             :accessor exponent))
  (:documentation "The number MANTISSA * $BASE^EXPONENT. There are no assumptions about the sizes or lengths of these quantities."))

(defun mpz-mpd (mpz)
  (make-instance 'mpd
    :mantissa mpz
    :exponent 0))

(defun mpd-zerop (mpd)
  "The MPD equal to zero?"
  (mpz-zerop (mantissa mpd)))

(defun mpd-oom (mpd)
  ;; OOM = Order of Magnitude
  ;;
  ;; This is defined as:
  ;;
  ;;            { -n   if x = 0.000[n digit zeros]00y
  ;;   oom(x) = {  
  ;;            {  0   otherwise
  ;;
  ;; Second value is a boolean indicating whether the number is zero.
  ;;
  ;; TODO FIXME: Is this correct?
  (cond
    ((mpd-zerop mpd)
     (values 0 t))
    (t
     (let ((s (mpz-size (mantissa mpd)))
           (e (exponent mpd)))
       (values (min 0 (+ s e)) nil)))))

(defmethod sign ((a mpd))
  (sign (mantissa a)))

(defun integer-mpd (n &optional (mantissa-class 'mpz/ram))
  (mpz-mpd (integer-mpz n mantissa-class)))

(defun mpd-rational (mpd)
  (* (mpz-integer (mantissa mpd))
     (expt $base (exponent mpd))))

(defun mpd-mpfr (x)
  (sb-mpfr:coerce (mpd-rational x) 'sb-mpfr:mpfr-float))

(defun mpd-truncate! (mpd &key to-digits to-bits)
  "Truncate the MPD to have at least TO-DIGITS digits or TO-BITS bits."
  (let* ((m (mantissa mpd))
         (s (mpz-size m))
         (digits
           (cond
             (to-digits to-digits)
             (to-bits (ceiling to-bits $digit-bits))
             (t (error "need to specify TO-DIGITS or TO-BITS")))))
    (unless (>= digits s)
      (let ((d (- s digits)))
        (left-displace-vec (storage m) d)
        (resize-vec-by (storage m) (- d))
        (incf (exponent mpd) d)
        nil))))

(defun mpd-negate (mpd)
  (make-instance 'mpd 
    :mantissa (mpz-negate (mantissa mpd))
    :exponent (exponent mpd)))

(defun mpd-half! (a)
  (check-type a mpd)
  ;; XXX: This assumes $base is divisible by 2.
  (mpz-multiply-by-digit! (/ $base 2) (mantissa a))
  (decf (exponent a)))

(defun mpd-* (a b)
  (make-instance 'mpd
    :mantissa (mpz-* (mantissa a)
                     (mantissa b))
    :exponent (+ (exponent a)
                 (exponent b))))

(defun mpd-+ (a b)
  (let ((a (mantissa a))
        (m (exponent a))
        (b (mantissa b))
        (n (exponent b)))
    (let ((min (min m n)))
      (make-instance 'mpd
        :mantissa (mpz-+ (mpz-left-shift a (* $digit-bits (- m min)))
                         (mpz-left-shift b (* $digit-bits (- n min))))
        :exponent min))))

(defun mpd-- (a b)
  (mpd-+ a (mpd-negate b)))

(defun mpd-integer-part (mpd)
  (let* ((e (exponent mpd))
         (m (mantissa mpd))
         (m-size (mpz-size m)))
    (cond
      ((not (plusp (+ m-size e)))
       (integer-mpz 0 'mpz/ram))
      ((zerop e)
       m)
      ((minusp e)
       (mpz-right-shift m (* $digit-bits (- e))))
      (t
       (mpz-left-shift m (* $digit-bits e))))))

