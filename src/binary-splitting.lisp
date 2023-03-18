;;;; binary-splitting.lisp
;;;;
;;;; Copyright (c) 2012-2019, 2022-2023 Robert Smith

;;; Canonical reference: https://www.ginac.de/CLN/binsplit.pdf

(in-package #:hypergeometrica)

;;; Representation of a hypergeometric series

(defun one (n)
  (declare (type integer n)
           (ignore n))
  (integer-mpz 1 'mpz/ram))

(defun int (n)
  (integer-mpz n 'mpz/ram))

(defstruct (series (:predicate series?))
  "A representation of the series

       ===
       \    a(k)   p(0) p(1) ... p(k)
        >  ------ --------------------
       /    b(k)   q(0) q(1) ... q(k)
       ===
       k>=0

Each of the function a, b, p, and q are integer-valued.
"
  (a #'one :type function :read-only t)
  (b #'one :type function :read-only t)
  (p #'one :type function :read-only t)
  (q #'one :type function :read-only t))

(defmethod print-object ((obj series) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

#+ignore
(defun product (f lower upper)
  "Compute the product

  F(LOWER) * F(LOWER + 1) * ... * F(UPPER - 1)."
  (declare (type fixnum lower upper))
  (labels ((rec (current accum)
             (if (>= current upper)
                 accum
                 (rec (1+ current)
                      (* accum (funcall f current))))))
    (rec lower 1)))

#+ignore
(defun sum-series-direct (series lower upper)
  (declare (type series series)
           (type fixnum lower upper))
  (assert (> upper lower))
  (loop :with a := (series-a series)
        :with b := (series-b series)
        :with p := (series-p series)
        :with q := (series-q series)
        :for n :from lower :below upper
        :sum (/ (* (funcall a n) (product p lower n))
                (* (funcall b n) (product q lower n)))))


;;; Partial sums

(defstruct (partial (:predicate partial?))
  "A partial sum of a series for LOWER <= k < UPPER."
  (lower nil :type fixnum :read-only t)
  (upper nil :type fixnum :read-only t)
  (p nil :type mpz/ram :read-only t)
  (q nil :type mpz/ram :read-only t)
  (b nil :type mpz/ram :read-only t)
  (r nil :type mpz/ram :read-only t))

(defmethod print-object ((obj partial) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "[~D, ~D)"
            (partial-lower obj)
            (partial-upper obj))))

(defun partial-numerator (x)
  (declare (type partial x))
  (partial-r x))

(defun partial-denominator (x)
  (declare (type partial x))
  (mpz-* (partial-b x) (partial-q x)))

;;; for debugging
(defun partial-digits (x digits)
  (declare (type partial x))
  (values (round (* (expt 10 digits) (mpz-integer (partial-numerator x)))
                 (mpz-integer (partial-denominator x)))))


;;; Binary splitting

(defun binary-split-base-case=1 (series lower &optional (upper (1+ lower)))
  (declare (type series series)
           (type fixnum lower upper))
  (assert (= 1 (- upper lower)))
  (let ((p (funcall (series-p series) lower)))
    (make-partial :lower lower
                  :upper upper
                  :p p
                  :q (funcall (series-q series) lower)
                  :b (funcall (series-b series) lower)
                  :r (mpz-* p (funcall (series-a series) lower)))))

(defun combine (left right)
  (declare (type partial left right))
  (assert (= (partial-upper left) (partial-lower right)))
  (make-partial :lower (partial-lower left)
                :upper (partial-upper right)
                :p (mpz-* (partial-p left) (partial-p right))
                :q (mpz-* (partial-q left) (partial-q right))
                :b (mpz-* (partial-b left) (partial-b right))
                :r (mpz-+ (mpz-* (partial-b right) (mpz-* (partial-q right) (partial-r left)))
                          (mpz-* (partial-b left)  (mpz-* (partial-p left)  (partial-r right))))))

(defun binary-split (series lower upper)
  (declare (type series series)
           (type fixnum lower upper))
  (assert (> upper lower))
  (let ((delta (- upper lower)))
    (cond
      ((= 1 delta) (binary-split-base-case=1 series lower upper))
      (t (let* ((m     (floor (+ lower upper) 2))
                (left  (binary-split series lower m))
                (right (binary-split series m upper)))
           (combine left right))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exponential eˣ

(defun make-exp-series (x)
  (check-type x rational)
  (let ((num (numerator x))
        (den (denominator x)))
    (make-series :a #'one
                 :b #'one
                 :p (lambda (n) (int (if (zerop n) 1 num)))
                 :q (lambda (n) (int (if (zerop n) 1 (* n den)))))))

(defun make-e-series ()
  (make-series :a #'one
               :b #'one
               :p #'one
               :q (lambda (n) (int (if (zerop n) 1 n)))))

(defun compute-e (prec)
  (let ((num-terms (+ 5 prec)))               ; way over-estimate
    (partial-digits (binary-split (make-e-series) 0 num-terms) prec)))

;;; Ramanujan's Series for pi

(defconstant +rama-decimals-per-term+ (log 96059301 10d0))
(defconstant +rama-a+ 1103)
(defconstant +rama-b+ 26390)
(defconstant +rama-c+ 396)

(defun make-ramanujan-series ()
  (flet ((a (n)
           (+ +rama-a+ (* n +rama-b+)))
         (p (n)
           (if (zerop n)
               1
               ;; This is Horner's form of
               ;; (2k - 1)*(4k - 3)*(4k - 1)
               (+ -3 (* n (+ 22 (* n (+ -48 (* n 32))))))))
         (q (n)
           (if (zerop n)
               1
               (* (expt n 3)
                  #.(/ (expt +rama-c+ 4) 8)))))
    (make-series :a (alexandria:compose #'int #'a)
                 :b #'one
                 :p (alexandria:compose #'int #'p)
                 :q (alexandria:compose #'int #'q))))

(defun compute-pi/ramanujan (prec)
  (let* ((num-terms (floor (+ 2 (/ prec +rama-decimals-per-term+))))
         (sqrt2 (isqrt (* 2 (expt 100 prec))))
         (num (* 2 2))
         (den (* (expt 99 2) sqrt2))
         (comp (binary-split (make-ramanujan-series) 0 num-terms)))
    (values (floor (* den (mpz-integer (partial-denominator comp)))
                   (* num (mpz-integer (partial-numerator comp)))))))




;;; Catalan's Constant G
;;;
;;; This doesn't actually compute Catalan's constant G. It would
;;; compute G' such that
;;;
;;;          3        π
;;;     G = --- G' + --- log(2 + √3)
;;;          8        8
;;;
(defun make-catalan-series ()
  (make-series :a #'one
               :b (lambda (n)
                    (1+ (* 2 n)))
               :p (lambda (n)
                    (if (zerop n) 1 n))
               :q (lambda (n)
                    (if (zerop n)
                        1
                        (+ 2 (* 4 n))))))


;;; Apéry's Constant ζ(3)

(defun make-apery-series ()
  (make-series :a (lambda (n)
                    (+ 77 (* n (+ 250 (* n 205)))))
               :b (lambda (n)
                    (declare (ignore n))
                    2)
               :p (lambda (n)
                    (if (zerop n)
                        1
                        (- (expt n 5))))
               :q (lambda (n)
                    (* 32 (expt (1+ (* 2 n)) 5)))))
