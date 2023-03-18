;;;; mpd.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

(defclass mpd ()
  ((mantissa :initarg :mantissa
             :accessor mantissa)
   (exponent :initarg :exponent
             :accessor exponent))
  (:documentation "The number MANTISSA * $BASE^EXPONENT"))

(defun mpz-mpd (mpz)
  (make-instance 'mpd
    :mantissa mpz
    :exponent 0))


(defun mpd-oom (mpd)
  ;; OOM = Order of Magnitude
  ;;
  ;; This is defined as:
  ;;
  ;;            { -n   if x = 0.000[n zeros]00y
  ;;   oom(x) = {  
  ;;            {  0   otherwise
  (let ((s (mpz-bit-size (mantissa mpd)))
        (e (* $digit-bits (exponent mpd))))
    (cond
      ((or (not (minusp e))
           (>= s (- e)))
       0)
      (t
       (+ s e)))))

(defmethod sign ((a mpd))
  (sign (mantissa a)))

(defun integer-mpd (n &optional (mantissa-class 'mpz/ram))
  (mpz-mpd (integer-mpz n mantissa-class)))

(defun mpd-rational (mpd)
  (* (mpz-integer (mantissa mpd))
     (expt $base (exponent mpd))))

(defun mpd-mpfr (x)
  (sb-mpfr:coerce (mpd-rational x) 'sb-mpfr:mpfr-float))

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

(defun mpd-truncate! (mpd to-digits)
  (let* ((m (mantissa mpd))
         (s (mpz-size m)))
    (unless (>= to-digits s)
      (let ((d (- s to-digits)))
        (left-displace-vec (storage m) d)
        (resize-vec-by (storage m) (- d))
        (incf (exponent mpd) d)
        nil))))

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



(defun estimate-reciprocal (a)
  (assert (not (mpz-zerop (mantissa a))))
  (let ((m (mantissa a))
        (e (exponent a)))
    (let* ((size (mpz-size m))
           (msb (with-vec (m m_)
                  (m_ (1- size)))))
      ;; 1/(a*2^e)
      ;; = (1/a)2^(-e)
      ;; ~ sign(a) * (1/msb(|a|) * 1/2^(len(a))) * 2^(-e)
      ;; ~ sign(a) * 1/msb(|a|) * 2^(-n-len(a))
      (let ((recip-mantissa-estimate
              (integer-mpz (round $base msb) 'mpz/ram)))
        (when (mpz-minusp m)
          (mpz-negate! recip-mantissa-estimate))
        (make-instance 'mpd
          :mantissa recip-mantissa-estimate
          :exponent (+ (- e)
                       (- size)))))))

(defun mpd-reciprocal (a bits)
  ;; Newton's method on f(x) = 1/x - a.
  ;;
  ;; Ordinarily the iteration is:
  ;;
  ;;     x(n+1) = 2*x(n) - a*x(n)^2
  ;;
  ;; We rewrite it by introducing a term:
  ;;
  ;;     h(n) = 1 - a*x(n)
  ;;
  ;;     x(n+1) = x(n) + x(n)*h(n).
  ;;
  ;; Since h(n) tends to zero as n tends to infinity, it serves as a
  ;; good estimate for the accuracy of x(n), allowing us to
  ;; confidently terminate the iteration.

  (loop :with x := (estimate-reciprocal a)
        :for iter-number :from 1
        :do (let* ((h (mpd-- (integer-mpd 1)
                             (mpd-* a x)))
                   (zeros (- (mpd-oom h))))
              (assert (plusp zeros) () "Got an estimate for 1/x which got *worse*!")
              (when *verbose*
                (format t "~&MPD-RECIPROCAL: iter=~D : h (2^-~D) = ~A~%"
                        iter-number
                        zeros
                        (mpd-mpfr h)))

              (setf x (mpd-+ x (mpd-* x h)))

              ;; Truncate the mantissa so that we land on DIGITs
              ;; without wasting memory or iterations.
              (cond
                ;; Have we exceeded the desired precision?
                ((> zeros bits)
                 (mpd-truncate! x bits) ; TODO: round?
                 (loop-finish))
                ;; Do we have less than 1/2 the precision left to go?
                ;; If so, we can actually do less work by lopping off
                ;; some precision now, since we'll double it again in
                ;; the next iteration.
                ;;
                ;; TODO: Should we truncate just a little bit less so
                ;; precision definitely goes over the expected amount?
                ((> (* 2 zeros) bits)
                 (mpd-truncate! x (floor bits 2)))
                ;; Truncate to the number of correct digits we've
                ;; found so far.
                (t
                 ;; Don't truncate to anything less than $DIGIT-BITS
                 ;; zeros. We don't actually save anything, for one,
                 ;; and for two, it can lead to infinite recursion.
                 (mpd-truncate! x (max $digit-bits zeros)))))
        :finally (return x)))


(defun test-recip (digits &optional (v 3))
  (let ((bits (ceiling (* digits (log 10.0d0 2.0d0)))))
    (sb-mpfr:set-precision (max (+ 8 bits) 250))
    (let ((x (mpd-reciprocal (integer-mpd v) bits)))
      (format t "~D digit~:P 1/~D = ~A~%" digits v (mpd-mpfr x)))))

(defun estimate-inv-sqrt (a)
  (let ((m (mantissa a))
        (e (exponent a)))
    (assert (not (mpz-minusp m)))
    (let* ((size (mpz-size m))
           (msb (with-vec (m m_)
                  (m_ (1- size)))))
      ;; 1/sqrt(a*B^e)
      ;; = 1/sqrt(a) * 1/sqrt(B^e)
      ;; = 1/sqrt(a) * B^(-e/2)
      ;; ~ 1/sqrt(msb(a) * B^(len(a) - 1)) * B^(-e/2)
      ;; = 1/sqrt(msb(a)) * 1/B^((len(a)-1)/2) * B^(-e/2)
      ;; = 1/sqrt(msb(a)) * B^([1 - len(a) - e]/2)
      ;; = B/sqrt(msb(a)) * B^([-1 - len(a) - e]/2)
      ;;
      ;; we make sure the B^*
      ;; exponent is even so that we
      ;; can divide by two
      ;; perfectly.
      (let* ((double-new-exp (+ -1 (- size) (- e)))
             (inv-sqrt-mantissa-estimate
               (if (oddp double-new-exp)
                   (integer-mpz (round $base (sqrt (* $base msb))) 'mpz/ram)
                   (integer-mpz (round $base (sqrt msb)) 'mpz/ram))))
        (when (oddp double-new-exp)
          (incf double-new-exp))
        (assert (evenp double-new-exp))
        (when (mpz-minusp m)
          (mpz-negate! inv-sqrt-mantissa-estimate))
        (make-instance 'mpd
          :mantissa inv-sqrt-mantissa-estimate
          :exponent (/ double-new-exp 2))))))

(defun mpd-inv-sqrt (a bits)
  ;; Newton's iteration for f(x) = 1/x^2 - a.
  ;;
  ;; This is ordinarily
  ;;
  ;;     x(n+1) = (3*x(n) - A*x(n)^3)/2
  ;;
  ;; but as with the reciprocal formula, we write it
  ;;
  ;;     h(n) = 1 - a*x(n)^2
  ;;
  ;;     x(n+1) = [2*x(n) + x(n)*h(n)]/2
  ;;
  ;; and estimate the error by the cancellation of h(n).
  (loop :with x := (estimate-inv-sqrt a)
        :for iter-number :from 1
        :do (let* ((h (mpd-- (integer-mpd 1)
                             (mpd-* a (mpd-* x x))))
                   (zeros (- (mpd-oom h))))
              (assert (plusp zeros) () "Got an estimate for 1/sqrt(x) which got *worse*!")

              (when *verbose*
                (format t "~&MPD-INV-SQRT: ~D : h (~D) = ~A~%"
                        iter-number
                        zeros
                        (mpd-mpfr h)))

              (setf x (mpd-+
                       (mpd-+ x x)
                       (mpd-* x h)))
              (mpd-half! x)

              ;; Truncate the mantissa so that we land on DIGITs
              ;; without wasting memory or iterations.
              (cond
                ((> zeros bits)
                 (mpd-truncate! x bits)   ; TODO: round?
                 (loop-finish))
                ((> (* 2 zeros) bits)
                 (mpd-truncate! x (floor bits 2)))
                (t
                 (mpd-truncate! x zeros))))
        :finally (return x)))

(defun mpd-sqrt (a bits)
  (mpd-* a (mpd-inv-sqrt a bits)))

(defun test-sqrt (digits &optional (v 2))
  (let ((bits (ceiling (* digits (log 10.0d0 2.0d0)))))
    (sb-mpfr:set-precision (max (+ 8 bits) 250))
    (let ((x (mpd-sqrt (integer-mpd v) bits)))
      (format t "~D digit~:P sqrt(~D) = ~A~%" digits v (mpd-mpfr x)))))
