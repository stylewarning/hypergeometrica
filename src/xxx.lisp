(in-package :hypergeometrica)

(defun x^x^x/ram (x)
  (mpz-expt (integer-mpz x) (expt x x)))

(defun x^x^x/disk (x)
  (declare (ignore x))
  (warn "not implemented")
  (integer-mpz 0))

(defun test-it (x)
  (let (ram disk)
    (format t "~&RAM &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
    (setf ram (time (x^x^x/ram x)))
    (format t "~&DISK &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
    (setf disk (time (x^x^x/disk x)))
    (sb-ext:gc :full t)
    (mpz-= ram disk)))

