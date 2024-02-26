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

(sb-ext:defglobal **communication-error-occured** nil)

(defun await-manager-reconnection ()
  (format t "Awaiting reconnection to a manager...")
  (let* ((lifeline (make-instance 'sb-bsd-sockets:local-socket :type :stream))
         (lifeline-node (merge-pathnames
                         (format nil "worker-~A-lifeline" **id**)
                         "/tmp/")))
    (sb-bsd-sockets:socket-bind lifeline (namestring lifeline-node))
    (sb-bsd-sockets:socket-listen lifeline 8)
    (format t "~2%!!! LIFELINE !!! Provide manager node to negotiate with to: ~S~2%" (namestring lifeline-node))
    (finish-output)
    (loop :named :REVIVED :do
      (let* ((client (sb-bsd-sockets:socket-accept lifeline))
             (stream (sb-bsd-sockets:socket-make-stream
                      client
                      :input t
                      :output t
                      :element-type 'character
                      :buffering :line))
             (message (read-form stream)))
        (format t "Lifeline message received: ~S~%" message)
        (sb-bsd-sockets:socket-close client)
        (when (typep message '(cons (member :revive)))
          (alexandria:destructuring-case message
            ((:revive &key id socket)
             (setf **id** id
                   **manager-address** (namestring socket))
             ;; TODO: We need to make sure we the manager knows what
             ;; state we are in before we carry on.
             (return-from :REVIVED))))))
    (sb-bsd-sockets:socket-close lifeline)))

(defmacro with-manager-io ((stream) &body body)
  (alexandria:with-gensyms (manager)
    `(bt:with-lock-held (**manager-lock**)
       (let ((,manager (make-instance 'sb-bsd-sockets:local-socket
                                      :type :stream)))
         (unwind-protect
              (tagbody
               :RETRY
                 (handler-case
                     (progn
                       (sb-bsd-sockets:socket-connect ,manager **manager-address**)
                       (let ((,stream (sb-bsd-sockets:socket-make-stream
                                       ,manager
                                       :element-type 'character
                                       :input t
                                       :output t
                                       :buffering ':line)))
                         ,@body))
                   (sb-bsd-sockets:socket-error (c)
                     (setf **comunication-error-occured** c)
                     (await-manager-reconnection)
                     (go :RETRY))))
           (when (sb-bsd-sockets:socket-open-p ,manager)
             (sb-bsd-sockets:socket-close ,manager)))))))

(defun write-form (stream form)
  (prin1 form stream)
  (terpri stream)
  (finish-output stream))

(defun read-form (stream)
  (read stream nil '(:eof)))

;;; Heartbeat

(defun make-heartbeat (&optional (period 5))
  (lambda ()
    (loop
      (with-manager-io (stream)
        (write-form stream `(,**id** :heartbeat))
        (finish-output stream))
      (sleep period))))

(sb-ext:defglobal **heartbeat-thread** nil)
(defun start-heartbeat-thread ()
  (setf **heartbeat-thread** (bt:make-thread (make-heartbeat)
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
           (format t "Received ID: ~D~%" id?)
           (setf **id** id?))
          (t
           (uiop:quit 1)))))

    ;; Start the heartbeat so we don't get kicked off.
    (start-heartbeat-thread)

    ;; Request and dispatch work.
    (with-manager-io (stream)
      (format t "#~D connected to socket.~%" **id**)
      (write-form stream `(,**id** :ping))
      (format t "#~D: received ~S~%" **id** (read-form stream))
      (finish-output))
    (bt:join-thread **heartbeat-thread**)))

(defun main ()
  (let ((app (cli-command)))
    (clingon:run app)))
