;;;; strandh-elster-reversal.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;;; This is an implementation of the Strandh-Elster bit reversal
;;;; algorithm. It is described in the following reference:
;;;;
;;;;     Robert Strandh and Anne C. Elster, "A Very Fast Recursive
;;;;     Bit-Reversal Algorithm", SIAM CSE'00: First SIAM Conference
;;;;     on Computational Science and Engineering, Washington, D.C.,
;;;;     Sep 21-24, 2000
;;;;
;;;; Since I do not have access to it, Prof. Strandh described it with
;;;; a reference implementation in the following link:
;;;;
;;;;     http://metamodular.com/bit-reversal.lisp


(defmacro with-reverted-operations (bindings &body body)
  (flet ((reverted-op (form)
           (ecase (car form)
             ;; ASH is not totally reversible. (We can shift off
             ;; values that can't be recovered without saving them.)
             ((ash) `(ash ,(second form) (- ,(third form))))
             ((+) `(- ,@(rest form)))
             ((-) `(+ ,@(rest form)))
             ;; LOGIOR is not totally reversible. (We can LOGIOR 1
             ;; into 1, which which can't be recovered without saving
             ;; them.)
             ((logior) `(logxor ,@(rest form))))))
    `(progn
       ,@(loop :for (var value-form) :in bindings
               :collect `(setf ,var ,value-form))
       ,@body
       ,@(loop :for (var value-form) :in bindings
               :collect `(setf ,var ,(reverted-op value-form)))
       (values))))

(defmacro do-non-symmetric-bit-reversals ((i j width &optional return) &body body)
  "Call the binary function F on numbers all numbers A and B such that:

    * A < B;
    * The bits of B are the reversal of the bits of A;
    * A and B are N or fewer bits wide.

Symmetric A and B are not included, and are not needed for most bit-reversal applications."
  (alexandria:with-gensyms (n b1 b2 all greater user)
    (multiple-value-bind (body decls doc) (alexandria:parse-body body :documentation nil)
      (declare (ignore doc))
      `(let* ((,n ,width)
              (,i 0)
              (,j 0)
              (,b1 0)
              (,b2 0))
         (declare (type (integer 0 64) ,n)
                  (type (unsigned-byte 64) ,i ,j ,b1 ,b2))
         (labels
             ((,user ()
                ,@decls
                ,@body
                (values))
              (,all ()
                (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
                (if (zerop ,n)
                    (,user)
                    (with-reverted-operations ((,b1 (ash ,b1 1))
                                               (,b2 (ash ,b2 -1)))
                      (if (= ,n 1)
                          (with-reverted-operations ((,n (- ,n 1)))
                            (,all)
                            (with-reverted-operations ((,i (logior ,i ,b1))
                                                       (,j (logior ,j ,b2)))
                              (,all)))
                          (with-reverted-operations ((,n (- ,n 2)))
                            (,all)
                            
                            ;; We avoid using WITH-REVERTED-OPERATIONS here for
                            ;; efficiency reasons. If we use that macro, then
                            ;; some of the reverted operations are then
                            ;; re-applied needlessly. We coalesce those here.
                            (locally ()
                              (setf ,i (logior ,i ,b1)
                                    ,j (logior ,j ,b2))
                              (,all)
                              
                              (setf ,i (logior ,i ,b2)
                                    ,j (logior ,j ,b1))
                              (,all)
                              (setf ,i (logxor ,i ,b1)
                                    ,j (logxor ,j ,b2))

                              (,all)
                              (setf ,i (logxor ,i ,b2)
                                    ,j (logxor ,j ,b1)))
                            #+#:equivalent
                            (progn
                              (with-reverted-operations ((,i (logior ,i ,b1))
                                                         (,j (logior ,j ,b2)))
                                (,all))

                              (with-reverted-operations ((,i (logior ,i ,b1 ,b2))
                                                         (,j (logior ,j ,b1 ,b2)))
                                (,all))

                              (with-reverted-operations ((,i (logior ,i ,b2))
                                                         (,j (logior ,j ,b1)))
                                (,all))))))))
              (,greater ()
                (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
                (with-reverted-operations ((,b1 (ash ,b1 1))
                                           (,b2 (ash ,b2 -1)))
                  (if (< ,n 4)
                      (with-reverted-operations ((,i (logior ,i ,b1))
                                                 (,j (logior ,j ,b2))
                                                 (,n (- ,n 2)))
                        (,all))
                      (with-reverted-operations ((,n (- ,n 2)))
                        (,greater)
                        
                        ;; We avoid using WITH-REVERTED-OPERATIONS here for
                        ;; efficiency reasons. If we use that macro, then
                        ;; some of the reverted operations are then
                        ;; re-applied needlessly. We coalesce those here.
                        (progn
                          (setf ,i (logior ,i ,b1)
                                ,j (logior ,j ,b2))
                          (,all)
                          
                          (setf ,i (logior ,i ,b2)
                                ,j (logior ,j ,b1))
                          (,greater)
                          
                          (setf ,i (logxor ,i ,b1 ,b2)
                                ,j (logxor ,j ,b1 ,b2)))
                        #+#:equivalent
                        (progn
                          (with-reverted-operations ((,i (logior ,i ,b1))
                                                     (,j (logior ,j ,b2)))
                            (,all))
                          (with-reverted-operations ((,i (logior ,i ,b1 ,b2))
                                                     (,j (logior ,j ,b1 ,b2)))
                            (,greater))))))))
           (declare (dynamic-extent (function ,greater)
                                    (function ,all)
                                    (function ,user))
                    (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
           (cond
             ((= 2 ,n)
              (setf ,i 1 ,j 2)
              (,user))
             ((= 3 ,n)
              (setf ,i 1 ,j 4)
              (,user)
              (setf ,i 3 ,j 6)
              (,user))
             ((< 3 ,n)
              ;; Avoid repeated reverted subtraction-by-2.
              (decf ,n 2)
              
              ;; Prepare for first call to ,GREATER.
              (setf ,b1 1
                    ,b2 (ash 1 (1+ ,n))
                    ,i 0
                    ,j 0)
              
              (,greater)

              ;; Prepare for call to ALL.
              (setf ,b1 1
                    ,b2 (ash 1 (1+ ,n))
                    ,i ,b1
                    ,j ,b2)
              
              (,all)

              ;; Prepare for second call to ,GREATER.
              (setf ,b1 1
                    ,b2 (ash 1 (1+ ,n))
                    ,i (logior ,b1 ,b2)
                    ,j ,i)
              
              (,greater)))
                         
           ;; Return.
           ,return)))))

(defun bit-reversed-permute! (x)
  "Permute the simple vector X of length 2^N in bit reversed order."
  (declare (type (simple-vector) x))
  (let* ((length (length x))
         (bits (integer-length (max 0 (1- length)))))
    ;; Check that this is a power of two.
    (assert (zerop (logand length (1- length))))
    (do-non-symmetric-bit-reversals (a b bits x)
      (rotatef (aref x a) (aref x b)))))
