;;;; solinas.lisp
;;;;
;;;; Copyright (c) 2019

(in-package #:hypergeometrica)

;;; "Generalized Mersenne Primes" by Jerome Solinas

(defun congruence-matrix (p)
  "Given a polynomial P as a vector, generate the congruence relations of

    t^(k + deg p)

for 0 <= k < deg p. This will be a matrix M[k, i] where i is the polynomial coefficient index for relation k. (This is the matrix that Solinas produces with a linear-feedback shift register)"
  (let* ((deg (1- (length p)))
         (c (make-array (list deg deg) :initial-element 0)))
    ;; Calculate the first congruence
    (let ((pre  (aref p deg)))
      (assert (= 1 pre))
      (loop :for i :below deg
            :do (setf (aref c 0 i) (* (/ pre) (- (aref p i))))))
    ;; Multiply everything else out
    (do-range (r 1 deg c)
      ;; Shift all terms (degreee < DEG) by t
      (do-range (i 1 deg)
        (setf (aref c r i) (aref c (1- r) (1- i))))
      ;; Add in the t^DEG term
      (let ((coef (aref c (1- r) (1- deg))))
        (dotimes (i deg)
          (incf (aref c r i) (* coef (aref c 0 i))))))))

(defun mod-matrix! (mat hom)
  (loop :for i :below (array-total-size mat)
        :do (setf #1=(row-major-aref mat i) (funcall hom #1#))
        :finally (return mat)))

(defun update-equations (p b a)
  "Produce update equations from the Solinas polynomial P and variables B and A. Updates will be on B from values of A."
  (check-type b symbol)
  (check-type a symbol)
  (check-type p vector)
  (let* ((c (congruence-matrix p))
         (deg (array-dimension c 0)))
    `(progn
       ,@(loop :for i :below deg
               :for bi := `(aref ,b ,i)
               :collect `(setf ,bi (+ (aref ,a ,i)
                                      ,@(loop :for j :below deg
                                              :for cji := (aref c j i)
                                              :for aj := `(aref ,a ,(+ deg j))
                                              :unless (zerop cji)
                                                :collect (cond
                                                           ((= 1 cji) aj)
                                                           ((= -1 cji) `(- ,aj))
                                                           (t `(* ,cji ,aj))))))))))

(defun solinas-polynomial (a m k)
  "Generate a Solinas polynomial from the prime a*2^m + 1 with t = 2^k."
  (let* ((p (1+ (* a (2^ m))))
         (bits (integer-length p)))
    (check-type p (unsigned-byte 64))
    (assert (primep p))
    (let* ((total-groups (ceiling bits k))
           (length (if (zerop (mod bits k))
                       (1+ total-groups)
                       total-groups))
           (poly (make-array length :initial-element 0)))
      ;; We always have a 2^bits term and a 1 term 
      (setf (aref poly 0) 1)
      (setf (aref poly (1- length)) 1)
      ;; Now we need a minus term. We combine this with 'a'.
      (multiple-value-bind (zero-groups slack-zeros) (floor m k)
        ;; polynomial indices < ZERO-GROUPS will be zero, except the first.
        ;;
        ;; SLACK-ZEROS is the number of zeros before A is considered
        (let ((az (ash a slack-zeros))
              (nontrivial-groups (- total-groups zero-groups)))
          ;; Now AZ := A * 2^(SLACK) accommodates the remaining
          ;; NONTRIVIAL-GROUPS. Go through all but the last one.
          (loop :repeat (1- nontrivial-groups)
                :for i :from zero-groups
                :do (setf (aref poly i) (ldb (byte k 0) az))
                    (setf az (ash az (- k))))
          (assert (<= 0 az (1- (2^ k))))
          ;; AZ contains the MSB of the original A of K bits. Now we
          ;; need to make this the one negative term.
          ;; 2^64 - c*2^48 = a*2^m
          ;;
          ;; 2^64 - a*2^m = c*2^48
          ;;
          ;; 2^16 - a*2^(m - 48) = c
          (setf (aref poly (- length 2)) (- az (2^ k)))))
      ;; return the poly
      poly)))

(defun evaluate-solinas-polynomial (poly k)
  (loop :for i :below (length poly)
        :sum (* (aref poly i) (2^ (* k i)))))
