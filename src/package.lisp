;;;; package.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(defpackage #:hypergeometrica
  (:use #:cl)
  ;; digit.lisp
  (:export
   #:$digit-bits                        ; CONSTANT
   #:$base                              ; CONSTANT
   #:$max-digit                         ; CONSTANT
   #:$digit-ones                        ; CONSTANT
   #:$largest-power-of-10-exponent      ; CONSTANT
   #:$largest-power-of-10               ; CONSTANT

   #:digit                              ; TYPE
   #:sign                               ; TYPE

   #:bytes-for-digits                   ; FUNCTION

   ;; wraparound arithmetic in radix $BASE
   #:fx+                                ; FUNCTION
   #:fx-                                ; FUNCTION
   #:fx1+                               ; FUNCTION
   #:fx1-                               ; FUNCTION
   #:fxneg                              ; FUNCTION
   #:fx*                                ; FUNCTION
   #:fx/                                ; FUNCTION

   #:ub64/2                             ; FUNCTION
   #:add64                              ; FUNCTION
   #:mul128                             ; FUNCTION
   #:div128                             ; FUNCTION
   #:add128                             ; FUNCTION
   #:sub128                             ; FUNCTION

   #:complement-digit                   ; FUNCTION
   )

  ;; modular-arithmetic.lisp
  (:export
   #:m+                                 ; FUNCTION
   #:m-                                 ; FUNCTION
   #:m1+                                ; FUNCTION
   #:m1-                                ; FUNCTION
   #:negate-mod                         ; FUNCTION
   #:inv-mod                            ; FUNCTION
   #:inv-mod/unsafe                     ; FUNCTION
   #:m/                                 ; FUNCTION
   #:expt-mod                           ; FUNCTION
   #:expt-mod/2^n                       ; FUNCTION
   #:expt-mod/safe                      ; FUNCTION
   #:mod128/fast                        ; FUNCTION
   #:m*/fast                            ; FUNCTION
   #:garner                             ; FUNCTION
   )

  ;; vec.lisp
  (:export
   #:vec-digit-pointer                  ; GENERIC
   #:vec-digit-length                   ; GENERIC
   #:copy-vec                           ; GENERIC
   #:resize-vec-by                      ; GENERIC
   #:free-vec                           ; GENERIC
   #:vec-ref                            ; GENERIC

   #:with-vec                           ; MACRO
   #:with-vecs                          ; MACRO
   #:vec->vector                        ; FUNCTION
   #:do-digits                          ; MACRO

   #:vec=                               ; FUNCTION
   #:vec-compare                        ; FUNCTION
   #:vec-fill                           ; FUNCTION
   #:vec-every                          ; FUNCTION
   #:vec-into                           ; FUNCTION
   #:vec-replace/unsafe                 ; FUNCTION

   #:vec-leading-zeros                  ; FUNCTION
   #:vec-trailing-zeros                 ; FUNCTION
   #:left-displace-vec                  ; FUNCTION
   #:vec-digit-length*                  ; FUNCTION
   )

  ;; ram-vec.lisp
  (:export
   #:ram-vec                            ; CLASS
   #:make-ram-vec                       ; FUNCTION
   #:sequence->ram-vec                  ; FUNCTION
   )

  ;; disk-vec.lisp
  (:export
   #:disk-vec                           ; CLASS
   #:make-disk-vec                      ; FUNCTION
   #:make-disk-vec-from-file            ; FUNCTION
   )
  )
