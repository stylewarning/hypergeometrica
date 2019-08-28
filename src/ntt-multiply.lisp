;;;; ntt-multiply.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(defun make-ntt-storage (n)
  (make-array n :element-type '(unsigned-byte 64)
                :initial-element 0))

(defun mpz-*/one-modulus (x y)
  (let* ((result-size (+ 1 (mpz-size x) (mpz-size y)))
         (result-storage (make-storage result-size))
         (length (least-power-of-two->= result-size))
         (result-ntt (make-ntt-storage length))
         (temp-ntt (make-ntt-storage length))
         (m (first (find-suitable-moduli (max length (expt $base 2)))))
         (w (ordered-root-from-primitive-root
             (find-primitive-root m)
             length
             m)))
    ;; Copy one of the factors and transform it
    (replace result-ntt (storage x))
    (setf result-ntt (ntt-forward result-ntt :modulus m :primitive-root w))

    ;; Copy
    (replace temp-ntt (storage y))
    ;; Transform
    (setf temp-ntt (ntt-forward temp-ntt :modulus m :primitive-root w))
    ;; Pointwise multiply
    (dotimes (i length)
      (setf (aref result-ntt i)
            (m* (aref result-ntt i) (aref temp-ntt i) m)))

    ;; Inverse transform
    (setf result-ntt (ntt-reverse result-ntt :modulus m :primitive-root w))

    ;; Unpack the result.
    (loop :with carry := 0
          :for i :below result-size
          :for ci := (+ carry (aref result-ntt i))
          :if (>= ci $base)
            :do (multiple-value-setq (carry ci) (floor ci $base))
          :else
            :do (setf carry 0)
          :do (setf (aref result-storage i) ci)
          :finally (assert (zerop carry))
                   (return (make-instance 'mpz :sign (* (sign x) (sign y))
                                               :storage result-storage)))))

(defun test-* (&rest xs)
  (let* ((p (apply #'mpz-* (mapcar #'integer-mpz xs)))
         (q (apply #'* xs)))
    (format t "actual: ~D~%" q)
    (format t "ntt   : ~D~%" (mpz-integer p))
    (format t "same: ~A~%" (= (mpz-integer p) q))
    p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; other ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ntt-coefficient ()
  'digit)

(defun count-trailing-zeroes (n)
  (assert (plusp n))
  (loop :for z :from 0
        :for x := n :then (ash x -1)
        :while (evenp x)
        :finally (return z)))

(defparameter *moduli*
  (sort (remove-duplicates
         (remove-if-not (lambda (m)
                          (<= 58 (integer-length m) (1- $digit-bits)))
                        (loop :for len :from 54 :to 60
                              :append (find-suitable-moduli (expt 2 len) :count 100))))
        #'>)
  "A set of moduli used to do transforms. They are sorted in decreasing order.")

;;; TODO: check that the transform lengths are compatible with this
(defparameter *max-transform-length-bits*
  (alexandria:extremum (mapcar (alexandria:compose #'count-trailing-zeroes #'1-) *moduli*) #'<)
  "The maximum lg(size) of a transform.")


(defun moduli-for-bits (bits)
  (labels ((get-em (moduli-collected moduli-left bits-remaining)
             (cond
               ((plusp bits-remaining)
                (assert (not (endp moduli-left)))
                (let ((mod (first moduli-left)))
                  (get-em (cons mod moduli-collected)
                          (rest moduli-left)
                          (- bits-remaining (integer-length mod)))))
               (t
                (nreverse moduli-collected)))))
    (get-em nil *moduli* bits)))

(defun modder (m)
  (lambda (n)
    (mod n m)))

(defun add-big-digit (digit storage i)
  (cond
    ((zerop digit) storage)
    ((>= i (length storage)) (error "Trying to add ~D at index ~D" digit i))
    (t (let ((sum (+ digit (aref storage i))))
         (multiple-value-bind (quo rem) (floor sum $base)
           (setf (aref storage i) rem)
           (add-big-digit quo storage (1+ i)))))))

(defun iterate (f x n)
  (assert (plusp n))
  (if (= 1 n)
      x
      (iterate f (funcall f x) (1- n))))

(defun make-ntt-work (mpz length moduli)
  (loop :for m :in moduli
        :for a := (make-array length :element-type 'ntt-coefficient :initial-element 0)
        :collect (map-into a (modder m) (storage mpz))))

(defun mpz-square (x)
  (let* ((size (mpz-size x))
         (length (least-power-of-two->= (* 2 size)))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (moduli (moduli-for-bits bound-bits))
         (roots (loop :for m :in moduli
                      :collect (ordered-root-from-primitive-root
                                (find-primitive-root m)
                                length
                                m)))
         (ntts (make-ntt-work x length moduli))
         ;; TODO don't allocate
         (result (make-array length :element-type 'ntt-coefficient :initial-element 0))
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                          (setf start-time (get-internal-real-time))
                          (finish-output)))))
    (format t "~&~%Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
            size
            (round (* size $digit-bits)
                   (log 10.0d0 2.00))
            (round (/ (* size $digit-bits) 8 1024 1024)))
    (format t "Transform length: ~D~%" length)
    (format t "Convolution bits: ~D~%" bound-bits)
    (format t "Moduli: ~{#x~16X~^, ~}~%" moduli)

    (format t "Forward")
    (loop :for m :in moduli
          :for w :in roots
          :for a :in ntts
          :do (ntt-forward a :modulus m :primitive-root w)
              (write-char #\.))
    (funcall report-time)

    ;; Pointwise multiply
    (format t "Pointwise multiply")
    (loop :for m :in moduli
          :for a :in ntts
          :do (dotimes (i length)
                (let ((ai (aref a i)))
                  (setf (aref a i) (m* ai ai m))))
              (write-char #\.))
    (funcall report-time)

    ;; Inverse transform
    (format t "Reverse")
    (loop :for m :in moduli
          :for w :in roots
          :for a :in ntts
          :do (ntt-reverse a :modulus m :primitive-root w)
              (write-char #\.))
    (funcall report-time)

    ;; Unpack the result.
    (format t "CRT...")
    (let* ((composite   (reduce #'* moduli))
           (complements (mapcar (lambda (m) (/ composite m)) moduli))
           (inverses    (mapcar #'inv-mod complements moduli))
           (factors     (mapcar #'* complements inverses)))
      (dotimes (i length)
        (loop :for a :in ntts
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) result i)))
      (funcall report-time))
    (make-instance 'mpz :storage result :sign 1)))

(defun mpz-* (x y)
  (let* ((size (+ (mpz-size x) (mpz-size y)))
         (length (least-power-of-two->= size))
         (bound-bits (integer-length (* length (expt (1- $base) 2))))
         (moduli (moduli-for-bits bound-bits))
         (roots (loop :for m :in moduli
                      :collect (ordered-root-from-primitive-root
                                (find-primitive-root m)
                                length
                                m)))
         (ntts-x (make-ntt-work x length moduli))
         (ntts-y (make-ntt-work y length moduli))
         ;; TODO don't allocate
         (result (make-array length :element-type 'ntt-coefficient :initial-element 0))
         (report-time (let ((start-time (get-internal-real-time)))
                        (lambda ()
                          (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time)) internal-time-units-per-second))
                          (setf start-time (get-internal-real-time))
                          (finish-output)))))
    (format t "~&~%Size: ~D (approx ~D decimal~:P, ~D MiB)~%"
            size
            (round (* size $digit-bits)
                   (log 10.0d0 2.00))
            (round (/ (* size $digit-bits) 8 1024 1024)))
    (format t "Transform length: ~D~%" length)
    (format t "Convolution bits: ~D~%" bound-bits)
    (format t "Moduli: ~{#x~16X~^, ~}~%" moduli)

    (format t "Forward")
    (loop :for m :in moduli
          :for w :in roots
          :for ax :in ntts-x
          :for ay :in ntts-y
          :do (ntt-forward ax :modulus m :primitive-root w)
              (write-char #\.)
              (ntt-forward ay :modulus m :primitive-root w)
              (write-char #\.))
    (funcall report-time)

    ;; Pointwise multiply. The NTT work for X is mutated.
    (format t "Pointwise multiply")
    (loop :for m :in moduli
          :for ax :in ntts-x
          :for ay :in ntts-y
          :do (dotimes (i length)
                (setf (aref ax i) (m* (aref ax i) (aref ay i) m)))
              (write-char #\.))
    (funcall report-time)

    ;; Inverse transform
    (format t "Reverse")
    (loop :for m :in moduli
          :for w :in roots
          :for ax :in ntts-x
          :do (ntt-reverse ax :modulus m :primitive-root w)
              (write-char #\.))
    (funcall report-time)

    ;; Unpack the result.
    (format t "CRT...")
    (let* ((composite   (reduce #'* moduli))
           (complements (mapcar (lambda (m) (/ composite m)) moduli))
           (inverses    (mapcar #'inv-mod complements moduli))
           (factors     (mapcar #'* complements inverses)))
      (dotimes (i length)
        (loop :for a :in ntts-x
              :for f :in factors
              :sum (* f (aref a i)) :into result-digit
              :finally (add-big-digit (mod result-digit composite) result i)))
      (funcall report-time))
    (make-instance 'mpz :storage result :sign (* (sign x) (sign y)))))
