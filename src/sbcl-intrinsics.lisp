;;;; sbcl-intrinsics.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

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

(in-package #:sb-vm)

(define-vop (hypergeometrica::ub64/2)
  (:translate hypergeometrica::ub64/2)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 6
    (move r x)
    (inst sar r 1)))

(define-vop (hypergeometrica::add64)
  (:translate hypergeometrica::add64)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target sum)
         (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num
              unsigned-num)
  (:results (sum   :scs (unsigned-reg) :from (:argument 0))
            (carry :scs (unsigned-reg)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move sum x)
    (zeroize carry)
    (inst add sum y)
    (inst set carry :c)))

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
  (:results (r-lo :scs (unsigned-reg) :from (:argument 0))
            (r-hi :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move rax x)
    (inst mul rax y)
    (move r-lo rax)
    (move r-hi rdx)))

;;; XXX: This won't properly detect overflow.
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

(define-vop (hypergeometrica::add128)
  (:translate hypergeometrica::add128)
  (:policy :fast-safe)
  (:args (a-lo :scs (unsigned-reg) :target c-lo)
         (a-hi :scs (unsigned-reg) :target c-hi)
         (b-lo :scs (unsigned-reg))
         (b-hi :scs (unsigned-reg)))
  (:arg-types unsigned-num
              unsigned-num
              unsigned-num
              unsigned-num)
  (:results (c-lo :scs (unsigned-reg) :from (:argument 0))
            (c-hi :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move c-lo a-lo)
    (move c-hi a-hi)
    (inst add c-lo b-lo)
    (inst adc c-hi b-hi)))

(define-vop (hypergeometrica::sub128)
  (:translate hypergeometrica::sub128)
  (:policy :fast-safe)
  (:args (a-lo :scs (unsigned-reg) :target c-lo)
         (a-hi :scs (unsigned-reg) :target c-hi)
         (b-lo :scs (unsigned-reg))
         (b-hi :scs (unsigned-reg)))
  (:arg-types unsigned-num
              unsigned-num
              unsigned-num
              unsigned-num)
  (:results (c-lo :scs (unsigned-reg) :from (:argument 0))
            (c-hi :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num
                 unsigned-num)
  (:generator 6
    (move c-lo a-lo)
    (move c-hi a-hi)
    (inst sub c-lo b-lo)
    (inst sbb c-hi b-hi)))
