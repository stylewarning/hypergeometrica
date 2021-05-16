;;; This file contains patches for the MMAP system.

(in-package #:mmap)

(cffi:defcenum (madvise-option :int)
  ;; Conventional advise values
  ;;
  ;; From /usr/include/asm-generic/mman-common.h
  (:madv-normal     0)
  (:madv-random     1)
  (:madv-sequential 2)
  (:madv-willneed   3)
  (:madv-dontneed   4))

(cffi:defcfun (u-madvise "madvise") :int
  (address :pointer)
  (length size-t)
  (advice madvise-option))

(defun madvise (addr size advice)
  (unless (zerop (u-madvise addr size advice))
    (error "madvise failed"))
  nil)
(export '(madvise) :mmap)
