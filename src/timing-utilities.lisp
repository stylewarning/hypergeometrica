;;;; timing-utilities.lisp
;;;;
;;;; Copyright (c) 2022 Robert Smith

(in-package #:hypergeometrica)

(defun delta-ms (start end)
  (round (* 1000 (- end start))
         internal-time-units-per-second))

(defmacro time* (&body body)
  (alexandria:with-gensyms (start result end)
    `(let* ((,start (get-internal-real-time))
            (,result (progn ,@body))
            (,end   (get-internal-real-time)))
       (values ,result (delta-ms ,start ,end)))))

(defmacro time! (place &body body)
  (alexandria:with-gensyms (delta result)
    `(multiple-value-bind (,result ,delta)
         (time* ,@body)
       (setf ,place ,delta)
       ,result)))

(defmacro with-stopwatch ((k &key (log '*verbose*) (stream '*standard-output*) (label "")) &body body)
  (alexandria:with-gensyms (start last gstream now glabel)
    `(let ((,gstream ,stream)
           (,glabel ,label)
           ,start ,last)
       (flet ((,k (message &rest args)
                (when ,log
                  (let ((,now (get-internal-real-time)))
                    (fresh-line ,gstream)
                    (format ,gstream "[~A~D Δ~D] "
                            ,glabel
                            (delta-ms ,start ,now)
                            (delta-ms ,last ,now))
                    (apply #'format ,gstream message args)
                    (terpri ,gstream)
                    (finish-output ,gstream)
                    (setf ,last ,now)
                    nil))))
         (setf ,start (get-internal-real-time)
               ,last  ,start)
         ,@body))))
