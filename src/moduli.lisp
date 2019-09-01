;;;; moduli.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; This file deals with modular arithmetic, as well as finding moduli
;;; suitable for number theoretic transforms.

;;;;;;;;;;;;;;;;;;;;;;;;; Modular Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $max-modulus (1- $base))
(deftype modulus ()
  `(integer 2 ,$max-modulus))

;; These are NOTINLINE'd below.
(declaim (ftype (function (digit digit modulus) digit) m+ m- m*))
(declaim (inline  m+ m- m* m/ m1+ m1- negate-mod inv-mod expt-mod))

(defun m- (a b m)
  "Compute A - B (mod M).

Assumes 0 <= A,B < M."
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (if (< a b)
      (the digit (+ (the digit (- m b)) a))
      (the digit (- a b))))

(defun m+ (a b m)
  "Compute A + B (mod M).

Assumes 0 <= A,B < M."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (inline m-))
  (if (zerop b)
      a
      (m- a (the digit (- m b)) m)))

(defun m1+ (a m)
  "Increment A modulo M.

Assumes 0 <= A < M."
  (let ((a (1+ a)))
    (if (= a m)
        0
        a)))

(defun m1- (a m)
  "Decrement A modulo M.

Assumes 0 <= A < M."
  (if (zerop a)
      (1- m)
      (1- a)))

(defun negate-mod (a m)
  "Negate A modulo M.

Assumes 0 <= A < M."
  (if (zerop a)
      0
      (- m a)))

;;; TODO: Figure out http://cacr.uwaterloo.ca/techreports/1999/corr99-39.pdf
(defun m* (a b m)
  (declare (type modulus m)
           (type digit a b))
  #+sbcl
  (multiple-value-bind (lo hi) (mul128 a b)
    (the digit (nth-value 1 (div128 lo hi m))))
  #-sbcl
  (mod (* a b) m))

(defun inv-mod (x m)
  "Compute X^-1 (mod M)."
  (labels ((egcd (x b a u)
             (if (zerop x)
                 (if (= 1 b)
                     (mod a m)
                     (error "~D is not invertible in Z/~DZ" x m)) ; X divides M
                 (multiple-value-bind (q r) (floor b x)
                   (egcd r x u (- a (* u q)))))))
    (egcd x m 0 1)))

(defun inv-mod/unsafe (x m)
  "Compute X^-1 (mod M). Assumes X is invertible."
  (declare (type digit x)
           (type modulus m)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (labels ((egcd (x b a u)
             (declare (type digit b a u x))
             (if (zerop x)
                 (mod a m)
                 (multiple-value-bind (q r) (floor b x)
                   (egcd r x u (the digit (- a (the digit (* u q)))))))))
    (egcd x m 0 1)))

(defun m/ (a b m)
  "Compute A / B = A * B^-1 (mod M)."
  (m* a (inv-mod b m) m))

(defmacro with-modular-arithmetic (m &body body)
  (check-type m (and (integer 2) (unsigned-byte 63)))
  `(labels ((cm+ (a b)
              (declare (type (unsigned-byte 64) a b)
                       (optimize speed (safety 0) (debug 0) (space 0))
                       (inline m+ m- m* m/))
              (m+ a b ,m))
            (cm- (a b)
              (declare (type (unsigned-byte 64) a b)
                       (optimize speed (safety 0) (debug 0) (space 0))
                       (inline m+ m- m* m/))
              (m- a b ,m))
            (cm* (a b)
              (declare (type (unsigned-byte 64) a b)
                       (optimize speed (safety 0) (debug 0) (space 0))
                       (inline m+ m- m* m/))
              (m* a b ,m))
            (cm/ (a b)
              (declare (type (unsigned-byte 64) a b)
                       (optimize speed (safety 0) (debug 0) (space 0))
                       (inline m+ m- m* m/))
              (m/ a b ,m)))
     (declare (inline cm+ cm- cm* cm/))
     ,@body))

(defun expt-mod (a n m)
  "Compute A ^ N (mod M) for integer N."
  (declare (type digit a)
           (type alexandria:non-negative-fixnum n)
           (type modulus m)
           (inline m*)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((result 1))
    (declare (type digit result))
    (loop
      (when (oddp n)
        (setf result (m* result a m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from expt-mod result))
      (setf a (m* a a m)))))

(declaim (inline expt-mod/2^n))
(defun expt-mod/2^n (a n m)
  "Compute A ^ (2 ^ N) (mod M) for integer N."
  (declare (type digit a)
           (type alexandria:non-negative-fixnum n)
           (type modulus m)
           (inline m*)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (dotimes (i n a)
    (setf a (m* a a m))))

(defun expt-mod/safe (a n m)
  "Compute A ^ N (mod M) for integer N."
  (when (minusp n)
    (setf a (inv-mod a m)
          n (- n)))

  (let ((result 1))
    (loop
      (when (oddp n)
        (setf result (mod (* result a) m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from expt-mod/safe result))
      (setf a (mod (* a a) m)))))

(declaim (notinline m+ m- m* m/ m1+ m1- negate-mod inv-mod expt-mod))


;;;;;;;;;;;;;;;;;;;;;; Primes and Factorization ;;;;;;;;;;;;;;;;;;;;;;

(defun factor-out (n f)
  "Return the values (A, B) such that N = A * F^B."
  (loop :with pow := 0
        :while (zerop (mod n f))
        :do (setf n (floor n f))
            (incf pow)
        :finally (return (values n pow))))

(defparameter *default-miller-rabin-count* 10
  "The default number of Miller-Rabin tests to perform.")

(defun primep (n &optional (k *default-miller-rabin-count*))
  "Is the positive integer N a prime?

This test uses the Miller-Rabin primality procedure. The positive integer K determines how many times this probabilistic test is run. Higher K implies a higher probability of correctness when PRIMEP returns T."
  (check-type n (integer 1))
  (check-type k (integer 1))
  (flet ((rand ()
           "Produce a random between 2 and N-2 inclusive."
           (+ 2 (random (- n 3)))))
    (cond ((= n 1)   nil)
          ((< n 4)     t)
          ((evenp n) nil)
          (t
           (multiple-value-bind (d s) (factor-out (- n 1) 2)
             (labels ((strong-liar? (a)
                        (let ((x (expt-mod/safe a d n)))
                          (or (= x 1)
                              (loop :repeat s
                                    :for y := x :then (mod (* y y) n)
                                      :thereis (= y (- n 1)))))))
               (declare (inline strong-liar?))
               (loop :repeat k
                     :always (strong-liar? (rand)))))))))

(defun next-prime (n)
  "Compute the next prime number after the positive integer N."
  ;; Try to be a little more clever by skipping evens.
  (let ((n (if (oddp n) n (1- n))))
    (loop :for i :from (+ n 2) :by 2
          :until (primep i)
          :finally (return i))))

(defun proth-primep (q n)
  "Test whether a number

    p = q * 2^n + 1

is a prime number."
  (declare (ignore q n))
  (error "not implemented"))

(defun factorize (number)
  "Compute the prime factorization of NUMBER. Return a list of conses (Pi . Ni) such that Pi are prime, Ni are positive integers, and the product of all Pi^Ni equals NUMBER."
  (check-type number (integer 2))
  (if (primep number)
      (list (cons number 1))
      (let ((factorization nil))
        (loop :for f := 2 :then (next-prime f)
              :until (= 1 number)
              :do (multiple-value-bind (next pow)
                      (factor-out number f)
                    (unless (zerop pow)
                      (push (cons f pow) factorization)
                      (setf number next)))
              :finally (return (nreverse factorization))))))

(defun factorization-string (factorization)
  "Generate a pretty printed string of the factorization FACTORIZATION as produced by `FACTORIZE'."
  (flet ((format-factor (f stream)
             (if (= 1 (cdr f))
                 (format stream "~D" (car f))
                 (format stream "~D^~D" (car f) (cdr f)))))
    (with-output-to-string (s)
      (loop :for f := (pop factorization)
            :while f :do
              (format-factor f s)
              (when factorization           ; there are more factors
                (write-string " * " s))))))


;;;;;;;;;;;;;;;;;;;;;;;;; Finding NTT Moduli ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; One key thing to compute number theoretic transforms is to compute
;;; suitable moduli. A "suitable modulus" M is a prime such that M =
;;; K*N + 1. For fast number theoretic transforms, N should be a power
;;; of 2, which will in turn be the bound on the length of our
;;; transform.
;;;
;;; Note that in reality, M does not need to be prime. We just get
;;; very nice properties from such a choice, e.g.
;;;
;;;    * Coprimality with the transform length (N must be invertible
;;;      for final normalization),
;;;
;;;    * Inversion for all elements < M in Z/MZ

(defun find-suitable-moduli (transform-length &key (count 1))
  "Return a list of COUNT number of suitable moduli for transforms of length TRANSFORM-LENGTH."
  (let* ((min-power (next-power-of-two transform-length))
         (min (expt 2 min-power))
         (moduli nil))
    (loop :for i :from 1
          :for p-1 := min :then (+ p-1 min)
          :for p := (1+ p-1)
          :while (plusp count)
          :do (when (primep p)
                (multiple-value-bind (a power) (factor-out (1- p) 2)
                  (declare (ignore a))
                  (when (>= power min-power)
                    (push p moduli)
                    (decf count))))
          :finally (return (nreverse moduli)))))

(defun modulus-factorization (modulus)
  "Pretty-print the factorization of the modulus MODULUS."
  (concatenate 'string "1 + " (factorization-string (factorize (1- modulus)))))

(defun print-suitable-moduli (transform-length count &key (stream *standard-output*)
                                                          max-waste
                                                          (print-function 'princ))
  "Print out up to COUNT suitable moduli for a desired transform length of TRANSFORM-LENGTH to the stream STREAM.

Specifically, the following will be printed:

    * In square brackets, the width of the word needed to represent the modulus and the number of wasted bits (a \"wasted bit\" is a bit that does not contribute to the transform length, i.e., the difference between the modulus length and the maximum power-of-two that divides the modulus minus 1),

    * The modulus M, and

    * The printed representation of the factorization of M - 1.

If MAX-WASTE is provided, then any moduli which have more than MAX-WASTE bits of waste will not be printed.
"
  (let ((moduli (find-suitable-moduli transform-length :count count))
        (max-waste (or max-waste most-positive-fixnum)))
    (dolist (modulus moduli)
      (let* ((width (next-power-of-two modulus))
             (waste (- width (nth-value 1 (factor-out (1- modulus) 2)))))
        (unless (> waste max-waste)
          (format stream "[~D,~D] "
                  width
                  waste)
          (funcall print-function modulus stream)
          (format stream " = ~A~%" (modulus-factorization modulus)))))))



;;;;;;;;;;;;;;;;;;;;;; Finding Primitive Roots ;;;;;;;;;;;;;;;;;;;;;;;

;;; Once we have a suitable modulus, we need to find primitive roots
;;; of unity for that modulus.

(defun generator-test-function (m)
  "Generate a unary function which tests if a value is a generator of the field Z/mZ. (This presumes that the modulus M forms a field.)"
  (let* ((m-1 (1- m))
         (prime-factors (mapcar #'car (factorize m-1)))
         (test-powers (mapcar (lambda (f) (floor m-1 f)) prime-factors)))
    (lambda (a)
      (loop :for power :in test-powers
            :never (= 1 (expt-mod/safe a power m))))))

(defun find-finite-field-generator (m)
  "Find the smallest generator of Z/mZ."
  (loop :with primitivep := (generator-test-function m)
        :for p := 2 :then (next-prime p)
        ;; :when (>= m p) :do (return-from find-primitive-root nil)
        :until (funcall primitivep p)
        :finally (return p)))

(defun primitive-root-from-generator (g n m)
  "Compute the primitive Nth root of unity from the generator G in Z/mZ.

Note: N must divide M - 1."
  (let ((k (/ (1- m) n)))
    (assert (integerp k))
    (expt-mod/safe g k m)))

(defun find-primitive-root (n m)
  "Find a primitive Nth root of unity for Z/mZ."
  (primitive-root-from-generator (find-finite-field-generator m) n m))

(defun naive-generator-p (g m)
  "Is G a generator of Z/mZ?"
  (let ((seen (make-array m :element-type 'bit :initial-element 0)))
    (loop :for i :below m
          :for x := g :then (m* g x m)
          :do (setf (sbit seen x) 1)
          :finally (progn (setf (sbit seen 0) 1)
                          (return (notany #'zerop seen))))))

(defun naive-primitive-root-p (w n m)
  "Is W a primitive Nth root of unity in Z/mZ?"
  (and (= 1 (expt-mod w n m))
       (loop :for k :from 1 :below n
             :always (zerop (loop :with s := 0
                                  :for j :below n
                                  :do (setf s (m+ s (expt-mod w (* j k) m) m))
                                  :finally (return s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > (print-suitable-moduli (expt 2 53) 15)
;; [58,3] 180143985094819841 = 1 + 2^55 * 5
;; [59,6] 459367161991790593 = 1 + 2^53 * 3 * 17
;; [60,7] 855683929200394241 = 1 + 2^53 * 5 * 19
;; [60,6] 882705526964617217 = 1 + 2^54 * 7^2
;; [60,7] 891712726219358209 = 1 + 2^53 * 3^2 * 11
;; [61,6] 1261007895663738881 = 1 + 2^55 * 5 * 7
;; [61,8] 1288029493427961857 = 1 + 2^53 * 11 * 13
;; [61,8] 1450159080013299713 = 1 + 2^53 * 7 * 23
;; [61,5] 1945555039024054273 = 1 + 2^56 * 3^3
;; [61,6] 2053641430080946177 = 1 + 2^55 * 3 * 19
;; [61,8] 2098677426354651137 = 1 + 2^53 * 233
;; [61,7] 2287828610704211969 = 1 + 2^54 * 127
;; [62,9] 2422936599525326849 = 1 + 2^53 * 269
;; [62,7] 2485986994308513793 = 1 + 2^55 * 3 * 23
;; [62,9] 2747195772696002561 = 1 + 2^53 * 5 * 61
;;
;; > (find-primitive-root 180143985094819841)
;; 11

(defun format-b64 (n &optional (stream *standard-output*))
  (check-type n (unsigned-byte 64))
  (dotimes (i 8)
    (format stream " ~8,'0B" (ldb (byte 8 (* 8 (- 8 i 1))) n))))

(defun rational-homomorphism (m)
  "Construct a homomorphism Q -> Z/mZ."
  (lambda (q)
    (let ((n (mod (abs (numerator q)) m))
          (d (mod (denominator q) m)))
      (* (signum q) (m/ n d m)))))

