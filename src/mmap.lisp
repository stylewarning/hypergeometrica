;;;; mmap.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(in-package #:hypergeometrica)

(defvar *mmaps* (tg:make-weak-hash-table :test 'eq :weakness ':key))

(defstruct (mmap-data (:copier nil)
                      (:constructor mmap-data (pointer fd size)))
  (pointer nil :read-only t)
  (fd      nil :read-only t)
  (size    nil :read-only t)
  ;; XXX: maybe keep the path here too...
  (unmapped nil))

(defmethod print-object ((o mmap-data) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (format stream "~D byte~:P" (mmap-data-size o))))

(defun mmap (path size)
  ;; XXX: should we create the file first instead of relying on MMAP?
  (let ((data (multiple-value-call #'mmap-data
                (mmap:mmap path :open '(:read :write :create :file-sync)
                                :protection '(:read :write)
                                :mmap '(:shared)
                                :size size))))
    (unless (<= size (mmap-data-size data))
      (munmap data)
      (error "bad mmap"))
    (setf (gethash data *mmaps*) (get-universal-time))
    (values data path)))

(defun munmap (data)
  (check-type data mmap-data)
  (unless (mmap-data-unmapped data)
    (mmap:munmap (mmap-data-pointer data)
                 (mmap-data-fd data)
                 (mmap-data-size data))
    (setf (mmap-data-unmapped data) t)
    (remhash data *mmaps*)
    nil))
