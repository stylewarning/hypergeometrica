(in-package :hypergeometrica)

(defun mpz->fbs (mpz)
  (check-type mpz mpz)
  (assert (typep (storage mpz) 'storage))
  (storage-to-file-backed-storage (storage mpz)))

(defun fbs->mpz (fbs)
  (check-type fbs file-backed-storage)
  (make-mpz 1
            (file-backed-storage-to-storage fbs)))

(defun integer-fbs (n)
  (mpz->fbs (integer-mpz n)))

(defun fbs-expt (fa n)
  (f-expt fa
          n
          (integer-fbs 1)
          (lambda (x y)
            (optimize-storage
             (fbs-* x y)))))

(defun x^x^x/ram (x)
  (mpz-expt (integer-mpz x) (expt x x)))

(defun x^x^x/disk (x)
  (optimize-storage (fbs->mpz (fbs-expt (integer-fbs x) (expt x x)))))

(defun test-it (x)
  (let (ram disk)
    (format t "~&RAM &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
    (setf ram (time (x^x^x/ram x)))
    (format t "~&DISK &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
    (setf disk (time (x^x^x/disk x)))
    (sb-ext:gc :full t)
    (mpz-= ram disk)))

(defun etest (a n)
  (let ((mpz (integer-mpz a))
        (fbs (integer-fbs a)))
    (setf mpz (mpz-expt mpz n))
    (format t "~&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
    (setf fbs (optimize-storage
               (fbs->mpz
                (fbs-expt fbs n))))
    (if (mpz-= mpz fbs)
        (format t "yes")
        (format t "no"))
    (sb-ext:gc :full t)
    (values
     (mpz-integer mpz)
     (mpz-integer fbs))))
