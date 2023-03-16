;;; pi.lisp
;;;
;;; Copyright (c) 2023 Robert Smith

(in-package #:hypergeometrica-tests)

(defun %test-pi (n &key (from 0)
                        (check (constantly t)))
  (loop :for k :from from :below n
        :for bits := (expt 10 k)
        :do
           (let (x-pi r-pi true-pi)
             (h::with-stopwatch (tim :log t :label "pi: ")
               (format t "~&Calculating pi with Hypergeometrica:~%")
               (setf x-pi (h::mpd-pi bits))
               (tim "hypergeometrica pi")

               (format t "~&Converting Hypergeometrica pi to MPFR:~%")
               (sb-mpfr:set-precision (+ bits 8))
               (setf r-pi (h::mpd-mpfr x-pi))
               (tim "hypergeometrica -> mpfr")

               (format t "~&Calculating MPFR pi:~%")
               (setf true-pi (sb-mpfr:const-pi))

               (format t "~&Calculated ~D bits [~D digits]~%" bits (round (* bits (log 2.0d0 10.0d0))))

               ;; TODO: make more efficient by comparing bits.
               (let ((diff (sb-mpfr:sub true-pi r-pi)))
                 (sb-mpfr:set-precision 64)
                 (let ((err (round (sb-mpfr:coerce (sb-mpfr:log2 diff) 'rational))))
                   (format t "==> error = ~A~2%" err)
                   (funcall check err)
                   (finish-output)))))))

(deftest test-pi ()
  (%test-pi 5 :from 1
              :check (lambda (n)
                       (is (zerop n)))))
