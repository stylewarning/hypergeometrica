(defpackage #:hypergeometrica/coalton
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:math #:coalton-library/math)
                    (#:list #:coalton-library/list)
                    (#:str #:coalton-library/string)))

(in-package #:hypergeometrica/coalton)

(named-readtables:in-readtable coalton:coalton)

;;; Raw memory primitives

(cl:declaim (cl:inline %malloc))
(cffi:defcfun (%malloc "malloc") :pointer
  (num-bytes :size))

(cl:declaim (cl:inline %free))
(cffi:defcfun (%free "free") :void
  (ptr :pointer))

(coalton-toplevel
  (define-type-alias Size UFix)
  (define-type-alias Index UFix)

  (repr :native cffi:foreign-pointer)
  (define-type Ptr)

  (inline)
  (declare ptr-read-u32 (Ptr -> Index -> U32))
  (define (ptr-read-u32 ptr i)
    (lisp U32 (ptr i)
      (cffi:mem-ref ptr ':uint32 (cl:* 4 i))))

  (inline)
  (declare ptr-write-u32! (Ptr -> Index -> U32 -> U32))
  (define (ptr-write-u32! ptr i new-value)
    (lisp U32 (ptr i new-value)
      (cl:setf (cffi:mem-ref ptr ':uint32 (cl:* 4 i)) new-value)))

  (inline)
  (declare ptr-read-u64 (Ptr -> Index -> U64))
  (define (ptr-read-u64 ptr i)
    (lisp U64 (ptr i)
      (cffi:mem-ref ptr ':uint64 (cl:* 8 i))))

  (inline)
  (declare ptr-write-u64! (Ptr -> Index -> U64 -> U64))
  (define (ptr-write-u64! ptr i new-value)
    (lisp U64 (ptr i new-value)
      (cl:setf (cffi:mem-ref ptr ':uint64 (cl:* 8 i)) new-value)))

  (declare alloc (Size -> Optional Ptr))
  (define (alloc num-bytes)
    (lisp (Optional Ptr) (num-bytes)
      (cl:let ((p (%malloc num-bytes)))
        (cl:if (cffi:null-pointer-p p)
               None
               (Some p)))))

  (inline)
  (declare free (Ptr -> Unit))
  (define (free p)
    (lisp Unit (p)
      (%free p)
      Unit))
  )


;;; Digit arithmetic
(coalton-toplevel
  (declare add64 (U64 -> U64 -> (Tuple U64 Bit)))
  (define (add64 a b)
    (lisp (Tuple U64 Bit) (a b)
      (cl:multiple-value-call #'Tuple
        (hypergeometrica:add64 a b))))

  (declare adc64 (Bit -> U64 -> U64 -> (Tuple U64 Bit)))
  (define (adc64 carry-in a b)
    (lisp (Tuple U64 Bit) (carry-in a b)
      (cl:let ((s (cl:+ carry-in a b)))
        (Tuple (cl:ldb (cl:byte 64 0) s)
               (cl:ldb (cl:byte 1 64) s)))))

  (declare mul128 (U64 -> U64 -> (Tuple U64 U64)))
  (define (mul128 a b)
    (lisp (Tuple U64 U64) (a b)
      (cl:multiple-value-call #'Tuple
        (hypergeometrica:mul128 a b)))))

(coalton-toplevel
  (define-struct Ctx
    ))

;;; MPN representation
(coalton-toplevel
  (define-struct ForeignNat
    (num-limbs Size)
    (pointer   Ptr))

  (define-type MPZ
    (MemMPZ I8 ForeignNat))

  (define (sign mpz)
    "The sign of the `mpz`."
    (match mpz
      ((MemMPZ s _) s)))

  #+ig
  (define-instance (Into  (List U64))
    (define (into x)
      (match x
        ((ImmNat x) (list:make x))
        ((RamNat size ptr)
         (rec % ((idx 0)
                 (digits Nil))
           (if (< idx size)
               (% (+ idx 8) (Cons (ptr-read-u64 ptr idx) digits))
               (list:reverse digits))))
        (_
         (undefined))))))

(defmacro RamNat* (cl:&rest digits)
  (cl:let ((len (cl:* 8 (cl:length digits)))
           (p (cl:gensym "PTR")))
    `(do
      (,p <- (alloc ,len))
       ,@(cl:loop
            :for i :from 0 :by 8
            :for digit :in digits
            :collect `(let _ = (ptr-write-u64! ,p ,i ,digit))) 
       (pure (RamNat ,len ,p)))))

;;; Addition
(coalton-toplevel
  (declare add-foreign-nat-into! (ForeignNat -> ForeignNat -> ForeignNat -> ForeignNat))
  (define (add-foreign-nat-into! dest a b)
    ;; invariant: size(dest) > max(size(a), size(b))
    (let a-size = (.num-limbs a))
    (let b-size = (.num-limbs b))
    (let overlap = (min a-size b-size))
    (let max =     (max a-size b-size))
    (let bigger-pointer = (if (> a-size b-size) (.pointer a) (.pointer b)))

    (let ((declare add-overlap (UFix -> Bit -> ForeignNat))
          (add-overlap
            (fn (i carry-in)
              (cond
                ((== i overlap)
                 (add-rest i carry-in))
                (True
                 (let (Tuple sum carry-out) = (adc64 carry-in
                                                     (ptr-read-u64 (.pointer a) i)
                                                     (ptr-read-u64 (.pointer b) i)))
                 (ptr-write-u64! (.pointer dest) i sum)
                 (add-overlap (1+ i) carry-out)))))
          (declare add-rest (UFix -> Bit -> ForeignNat))
          (add-rest
            (fn (i carry-in)
              (cond
                ((== i max)
                 (ptr-write-u64! bigger-pointer i (into carry-in))
                 dest)
                (True
                 (let (Tuple sum carry-out) = (adc64 carry-in
                                                     (ptr-read-u64 bigger-pointer i)
                                                     0))
                 (ptr-write-u64! (.pointer dest) i sum)
                 (add-rest (1+ i) carry-out))))))
      (add-overlap 0 0))))
#+ig
(coalton-toplevel
  (define (add x y)
    (match (Tuple x y)
      ((Tuple (ImmNat x) (ImmNat y))
       (let (Tuple s c) = (add64 x y))
       (if (zero? c)
           (ImmNat s)
           (unwrap (RamNat* s (into c)))))
      ((Tuple (RamNat _x _px) (RamNat _y _py))
       (undefined))
      (_
       (undefined)))))
