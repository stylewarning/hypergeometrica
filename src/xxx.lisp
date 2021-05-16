(in-package :hypergeometrica)

(defun x^x^x/ram (x)
  (mpz-expt (integer-mpz x) (expt x x)))

(defun x^x^x/disk (x)
  (let ((*maximum-vector-size* 0))
    (mpz-expt (integer-mpz x) (expt x x))))

(defun test-it (x)
  (let ((*verbose* t))
    (let (ram disk)
      (format t "~&RAM &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
      (setf ram (time (x^x^x/ram x)))
      (format t "~&DISK &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~%")
      (setf disk (time (x^x^x/disk x)))
      (sb-ext:gc :full t)
      (mpz-= ram disk))))

