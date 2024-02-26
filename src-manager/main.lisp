;;;; Copyright (c) 2024 Robert Smith

(in-package #:hypergeometrica-manager)

(sb-ext:defglobal **socket** nil)
(sb-ext:defglobal **socket-node** nil)
(sb-ext:defglobal **socket-thread** nil)

;;; Socket

(defun write-form (stream form)
  (prin1 form stream)
  (terpri stream)
  (finish-output stream))

(defun make-socket-listener ()
  (let* ((server **socket**))
    (lambda ()
      (unwind-protect
           (loop
             (let* ((client (sb-bsd-sockets:socket-accept server))
                    (stream (sb-bsd-sockets:socket-make-stream
                             client
                             :input t
                             :output t
                             :element-type 'character
                             :buffering :line
                             :timeout 5))
                    (message (read stream nil '(:eof))))
               (handle-worker-message message stream)
               (sb-bsd-sockets:socket-close client)))
        (sb-bsd-sockets:socket-close **socket**)
        (delete-file **socket-node**)
        (setf **socket** nil
              **socket-node** nil
              **socket-thread** nil)))))

(defun start-socket-thread ()
  (when **socket-thread**
    (warn "Socket thread already started.")
    (bt:destroy-thread **socket-thread**))
  (setf **socket-thread** (bt:make-thread
                           (make-socket-listener)
                           :name "Hypergeometrica Socket Server")))

;;; Worker Tracking

(defclass worker-status ()
  ((id :accessor worker-status-id
       :initarg :id)
   (last-heartbeat :accessor last-heartbeat
                   :initarg :last-heartbeat)))

(sb-ext:defglobal **max-workers** 1)
(sb-ext:defglobal **workers-lock** (bt:make-lock "**workers**"))
(sb-ext:defglobal **workers** nil)

(defun make-id ()
  (sleep 1.5)
  (get-universal-time))

(defun check-worker (id)
  (bt:with-lock-held (**workers-lock**)
    (let ((status (find id **workers** :key #'worker-status-id)))
      (cond
        (status
         (setf (last-heartbeat status) (get-internal-real-time))
         id)
        (t
         (warn "Unknown worker identified as #~D" id)
         nil)))))

;;; Lifelines

(defun revive-lifelines (lifelines)
  (dolist (lifeline lifelines)
    (cond
      ((probe-file lifeline)
       (handler-case
           (let ((lifeline-socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
             (unwind-protect
                  (progn
                    (sb-bsd-sockets:socket-connect lifeline-socket lifeline)
                    (let ((stream (sb-bsd-sockets:socket-make-stream
                                   lifeline-socket
                                   :element-type 'character
                                   :input t
                                   :output t
                                   :buffering ':line)))
                      (let ((new-id (make-id)))
                        (push
                         (make-instance 'worker-status
                                        :id new-id
                                        :last-heartbeat (get-internal-real-time))
                         **workers**)
                        (format t "Reviving: lifeline ~A -> ~S~%" lifeline new-id)
                        (write-form stream `(:revive :id ,new-id :socket ,**socket-node**))
                        ;; TODO: bookkeeping on current computational progress
                        )))
               (sb-bsd-sockets:socket-close lifeline-socket)))
         (sb-bsd-sockets:socket-error (c)
           (declare (ignore c))
           (warn "Error communicating with lifeline socket ~A... skipping" lifeline))))
      (t
       (warn "Invalid lifeline: ~A" lifeline)))))

;;; Heartbeat

(defun make-heartbeat-checker (&optional (timeout 10))
  (lambda ()
    (loop
      (sleep timeout)
      (bt:with-lock-held (**workers-lock**)
        (loop :for status :in **workers**
              :if (< timeout (/ (- (get-internal-real-time)
                                   (last-heartbeat status))
                                internal-time-units-per-second))
                :collect status :into evict
              :else
                :collect status :into renew
              :finally (progn
                         (setf **workers** renew)
                         (dolist (status evict)
                           (warn "Evicting ~A due to timeout." (worker-status-id status)))))))))

(sb-ext:defglobal **heartbeat-checker-thread** nil)
(defun start-heartbeat-checker-thread ()
  (setf **heartbeat-checker-thread**
        (bt:make-thread (make-heartbeat-checker) :name "Heartbeat Checker")))

;;; Worker Message Handling

(defun handle-unknown-message (message)
  (warn "Unknown message received: ~A" (prin1-to-string message))
  nil)

(defun handle-worker-message (message stream)
  (typecase message
    (atom
     (handle-unknown-message message))
    ((cons keyword)
     (alexandria:destructuring-case message
       ((:eof)
        (warn "Received EOF from client."))
       ((:join)
        (bt:with-lock-held (**workers-lock**)
          (cond
            ((> **max-workers** (length **workers**))
             (let ((new-id (make-id)))
               (push
                (make-instance 'worker-status
                               :id new-id
                               :last-heartbeat (get-internal-real-time))
                **workers**)
               (write-form stream `(:welcome :id ,new-id))))
            (t
             (write-form stream '(:no-vacancy))))))
       ((:status)
        nil)
       ((t &rest rest)
        (declare (ignore rest))
        (handle-unknown-message message))))
    (t
     (let ((from (car message)))
       (when (check-worker from)
         (alexandria:destructuring-case (cdr message)
           ((:ping)
            (format t "Ping from client ~D~%" from)
            (write-form stream '(:pong))
            (finish-output stream))
           ((:heartbeat)
            (format t "Heartbeat from worker #~D~%" from))
           ((t &rest rest)
            (declare (ignore rest))
            (handle-unknown-message message))))))))

;;; CLI

(defun cli-options ()
  (list
   (clingon:make-option
    :integer
    :required t
    :description "maximum number of workers"
    :long-name "max-workers"
    :key :max-workers)
   (clingon:make-option
    :list
    :description "lifelines to restart work with"
    :long-name "lifeline"
    :key :lifelines)))

(defun cli-command ()
  (clingon:make-command
   :name "hypergeometrica-manager"
   :options (cli-options)
   :handler #'cli-handler))

(defun cli-handler (cmd)
  (let ((pid (sb-posix:getpid)))
    (setf **max-workers** (clingon:getopt cmd ':max-workers)
          **socket** (make-instance 'sb-bsd-sockets:local-socket
                                    :type :stream)
          **socket-node** (merge-pathnames
                           (format nil "manager-~D" pid)
                           "/tmp/"))
    (sb-bsd-sockets:socket-bind **socket** (namestring **socket-node**))
    (sb-bsd-sockets:socket-listen **socket** 8)
    (start-heartbeat-checker-thread)
    (revive-lifelines (clingon:getopt cmd ':lifelines))
    (start-socket-thread)
    (format t "Started socket on: ~A~%" **socket-node**)
    (format t "Waiting for socket thread to end.~%")
    (finish-output)
    (bt:join-thread **socket-thread**)))

(defun main ()
  (sb-ext:disable-debugger)
  (let ((app (cli-command)))
    (clingon:run app)))
