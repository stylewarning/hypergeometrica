(in-package #:hypergeometrica-debug)

(defun dd (mpd)
  "Print out an MPD using MPFR."
  (format t "~A~%" (sb-mpfr:coerce (h::mpd-rational mpd) 'sb-mpfr:mpfr-float))
  mpd)
