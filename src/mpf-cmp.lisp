;;;; mpf-cmp.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defun mpf-cmp-abs (a b)
  "Compare the absolute values of two numbers."
  (cond
    ((/= (mpf-exponent a) (mpf-exponent b))
     (if (< (mpf-exponent a) (mpf-exponent b))
         cmp/lt
         cmp/gt))
    (t
     (let* ((a-len (vec-digit-length (mpf-storage a)))
            (b-len (vec-digit-length (mpf-storage b)))
            (len (max a-len b-len)))
       (loop :for i :from (1- len) :downto 0
             :for va := (mpf-digit a (1+ (- a-len len)))
             :for vb := (mpf-digit b (1+ (- b-len len)))
             :when (/= va vb)
               :return (if (< va vb) cmp/lt cmp/gt)
             :finally (return cmp/eq))))))

(defun mpf-cmp-total (a b)
  ;;     NAN = NAN
  ;;
  ;; not-NAN < NAN
  ;;
  ;;      -0 < 0
  ;;
  ;;
  ;; TODO: simplify
  (cond
    ((or (mpf-nan? a) (mpf-nan? b))
     (cond
       ((= (mpf-exponent a) (mpf-exponent b))
        cmp/eq)
       ((mpf-nan? a)
        cmp/gt)
       (t
        cmp/lt)))
    ((< (mpf-sign a) (mpf-sign b))
     cmp/lt)
    ((> (mpf-sign a) (mpf-sign b))
     cmp/gt)
    (t
     (let ((abs-cmp (mpf-cmp-abs a b)))
       (if (plusp (mpf-sign a))
           abs-cmp
           (flip-comparison abs-cmp))))))

(defun mpf-cmp (a b)
  (cond
    ((or (mpf-nan? a) (mpf-nan? b))
     cmp/??)
    ((/= (mpf-sign a) (mpf-sign b))
     (cond
       ((and (mpf-zero? a) (mpf-zero? b))
        cmp/eq)
       ((< (mpf-sign a) (mpf-sign b))
        cmp/lt)
       (t
        cmp/gt)))
    (t
     (let ((abs-cmp (mpf-cmp-abs a b)))
       (if (plusp (mpf-sign a))
           abs-cmp
           (flip-comparison abs-cmp))))))
