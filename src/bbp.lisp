;;; bbp.lisp

(in-package #:hypergeometrica)

;;; Sketched out implementation of the BBP algorithm using normal Lisp
;;; INTEGERs.

(defun modpow (a n m)                   ; EXPT-MOD without type restrictions
  (let ((result 1))
    (loop
      (when (oddp n)
        (setf result (mod (* result a) m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from modpow result))
      (setf a (mod (* a a) m)))))

(defun bbp (n &key (precision 14))
  (let* ((bits (* 4 precision)))
    (labels ((mask (x)
               (ldb (byte bits 0) x))
             (s (j n)
               (let ((sum 0))
                 (loop :for k :below n
                       :for r := (+ j (* 8 k))
                       :do (incf sum (floor
                                      (ash (modpow 16 (- n k 1) r) bits)
                                      r))
                           (setf sum (mask sum)))

                 (loop :for k :from n
                       :for i :from (1- precision) :downto 0
                       :for d := (floor (expt 16 i)
                                        (+ j (* 8 k)))
                       :until (zerop d)
                       :do (incf sum d))
                 sum)))
      (mask
       (- (* 4 (s 1 n))
          (* 2 (s 4 n))
          (* 1 (s 5 n))
          (* 1 (s 6 n)))))))
