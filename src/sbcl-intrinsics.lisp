;;;; sbcl-intrinsics.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

(sb-c:defknown mul128 ((unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown div128 ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(in-package #:sb-vm)

(define-vop (hypergeometrica::mul128)
  (:translate hypergeometrica::mul128)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target rax)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num
              unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :target r-lo
               :from (:argument 0) :to (:result 0))
              rax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target r-hi
               :from :eval :to (:result 1))
              rdx)
  (:results (r-lo :scs (unsigned-reg))
            (r-hi :scs (unsigned-reg)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move rax x)
    (inst mul rax y)
    (move r-lo rax)
    (move r-hi rdx)))

(define-vop (hypergeometrica::div128)
  (:translate hypergeometrica::div128)
  (:policy :fast-safe)
  (:args (dividend-lo :scs (unsigned-reg) :target rax)
         (dividend-hi :scs (unsigned-reg) :target rdx)
         (divisor :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num
              unsigned-num
              unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset :target quotient
               :from (:argument 0) :to (:result 0))
              rax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target remainder
               :from (:argument 1) :to (:result 1))
              rdx)
  (:results (quotient :scs (unsigned-reg))
            (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move rax dividend-lo)
    (move rdx dividend-hi)
    (inst div rax divisor)
    (move quotient rax)
    (move remainder rdx)))
