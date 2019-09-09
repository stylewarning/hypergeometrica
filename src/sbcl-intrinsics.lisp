;;;; sbcl-intrinsics.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; In this file, we just define known functions for SBCL.

(sb-c:defknown ub64/2 ((unsigned-byte 64))
    (unsigned-byte 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown add64 ((unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) bit)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown mul128 ((unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown div128 ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown add128 ((unsigned-byte 64) (unsigned-byte 64)
                       (unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown sub128 ((unsigned-byte 64) (unsigned-byte 64)
                       (unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)
