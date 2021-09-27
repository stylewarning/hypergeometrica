;;;; mpf-ctor.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun mpf-zero ()
  (make-mpf 1 $nul-exponent-value (make-ram-vec 0)))

(defun mpf-nan ()
  (make-mpf 1 $nan-exponent-value (make-ram-vec 0)))

(defun mpf-inf ()
  (make-mpf 1 $inf-exponent-value (make-ram-vec 0)))

(defun mpf-set-nan! (r)
  (declare (type mpf r))
  (resize-vec-to (mpf-storage r) 0)
  (setf (mpf-exponent r) $nan-exponent-value)
  (setf (mpf-sign r) 1)
  nil)

(defun mpf-set-zero! (r &key (sign 1))
  (declare (type mpf r)
           (type sign sign))
  (resize-vec-to (mpf-storage r) 0)
  (setf (mpf-exponent r) $nul-exponent-value)
  (setf (mpf-sign r) sign)
  nil)

(defun mpf-set-inf! (r &key (sign 1))
  (declare (type mpf r)
           (type sign sign))
  (resize-vec-to (mpf-storage r) 0)
  (setf (mpf-exponent r) $inf-exponent-value)
  (setf (mpf-sign r) sign)
  nil)

(defun mpf-set-ui! (r a)
  (declare (type mpf r)
           (type (unsigned-byte 64) a))
  (setf (mpf-sign r) 1)
  (cond
    ((zerop a)
     (setf (mpf-exponent r) $nul-exponent-value)
     (resize-vec-to (mpf-storage r) 0))
    (t
     (resize-vec-to (mpf-storage r) 1)
     (let ((shift (clz a)))
       (setf (vec-ref (mpf-storage r) 0) (ash a shift))
       (setf (mpf-exponent r) (- $digit-bits shift)))))
  nil)

(defun mpf-set-si! (r a)
  (declare (type mpf r)
           (type (signed-byte 64) a))
  (cond
    ((minusp a)
     (mpf-set-ui! r (- a))
     (setf (mpf-sign r) (- (mpf-sign r))))
    (t
     (mpf-set-ui! r a)))
  nil)

(defun mpf-set! (r a)
  (declare (type mpf r a))
  (cond
    ((eq r a) nil)
    (t
     (resize-vec-to (mpf-storage r) (vec-digit-length (mpf-storage a)))
     (setf (mpf-sign r) (mpf-sign a)
           (mpf-exponent r) (mpf-exponent a))
     (vec-replace/unsafe (mpf-storage r) (mpf-storage a))
     nil)))

(defun integer-mpf (n)
  (cond
    ((zerop n) (mpf-zero))
    (t
     (let* ((sign (signum n))
            (abs-n (abs n))
            (bits (integer-length abs-n)))
       (multiple-value-bind (digits slack) (ceiling bits $digit-bits)
         ;; slack will be NON-POSITIVE
         ;;
         ;; get N to be "aligned" to $DIGIT-BITS, starting from the
         ;; MSB (i.e., we want to add bits to the LSB, not the MSB).
         (setf abs-n (ash abs-n (- slack)))
         (let ((s (make-storage digits)))
           (with-vec (s s_)
             (loop :for i :below digits
                   :for x := abs-n :then (ash x (- $digit-bits))
                   :for digit := (ldb (byte $digit-bits 0) x)
                   :do (setf (s_ i) digit)))
           (make-mpf sign bits s)))))))
