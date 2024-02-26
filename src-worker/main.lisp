;;;; Copyright (c) 2024 Robert Smith

(in-package #:hypergeometrica-worker)

(sb-ext:defglobal **id** nil)
(sb-ext:defglobal **manager-address** nil)
(sb-ext:defglobal **manager-lock** (bt:make-lock))

(defun cli-options ()
  (list
   (clingon:make-option
    :string
    :description "Address of open socket."
    :short-name #\s
    :long-name "socket-address"
    :key :socket-address)
   ))

(defun cli-command ()
  (clingon:make-command
   :name "hypergeometrica-worker"
   :options (cli-options)
   :handler #'cli-handler))

(defmacro with-manager-io ((stream) &body body)
  (alexandria:with-gensyms (manager)
    `(bt:with-lock-held (**manager-lock**)
       (let ((,manager (make-instance 'sb-bsd-sockets:local-socket
                                     :type :stream)))
         (unwind-protect
              (progn
                (sb-bsd-sockets:socket-connect ,manager **manager-address**)
                (let ((,stream (sb-bsd-sockets:socket-make-stream
                                ,manager
                                :element-type 'character
                                :input t
                                :output t
                                :buffering ':line)))
                  ,@body))
           (when (sb-bsd-sockets:socket-open-p ,manager)
             (sb-bsd-sockets:socket-close ,manager)))))))

(defun write-form (stream form)
  (prin1 form stream)
  (terpri stream)
  (finish-output stream))

(defun read-form (stream)
  (read stream nil '(:eof)))

;;; Heartbeat

(defun make-heartbeat (id &optional (period 5))
  (let ((heartbeat `(,id :heartbeat)))
    (lambda ()
      (loop
        (with-manager-io (stream)
          (write-form stream heartbeat)
          (finish-output stream))
        (sleep period)))))

(sb-ext:defglobal **heartbeat-thread** nil)
(defun start-heartbeat-thread ()
  (setf **heartbeat-thread** (bt:make-thread (make-heartbeat **id**)
                                             :name "Heartbeat Thread")))

;;; Request ID

(defun request-id (stream)
  (write-form stream '(:join))
  (alexandria:destructuring-ecase (read-form stream)
    ((:welcome &key id)
     (format t "Got an ID: ~A~%" id)
     id)
    ((:no-vacancy)
     (format t "No vacancy.~%")
     nil)))

;;; Main

(defun cli-handler (cmd)
  (let ((manager-address (clingon:getopt cmd ':socket-address)))
    ;; Set the manager address.
    (unless (probe-file manager-address)
      (error "Manager address ~A not found." manager-address))
    (setf **manager-address** manager-address)

    ;; Get an ID.
    (with-manager-io (stream)
      (format t "Requesting ID.~%")
      (let ((id? (request-id stream)))
        (cond
          (id?
           (setf **id** id?))
          (t
           (uiop:quit 1)))))

    (start-heartbeat-thread)
    (with-manager-io (stream)
      (format t "#~D connected to socket.~%" **id**)
      (write-form stream `(,**id** :ping))
      (format t "#~D: received ~S~%" **id** (read-form stream))
      (finish-output))
    (bt:join-thread **heartbeat-thread**)))

(defun main ()
  (let ((app (cli-command)))
    (clingon:run app)))
