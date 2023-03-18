;;;; hypergeometrica.asd
;;;;
;;;; Copyright (c) 2019-2023 Robert Smith

(asdf:defsystem #:hypergeometrica
  :description "Calculate lots of digits of things."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:alexandria
               #:global-vars
               #:lparallel
               #:napa-fft3
               #:trivial-garbage
               #:uiop

               #:cffi
               #:mmap

               (:feature :sbcl #:sb-mpfr)
               (:feature :sbcl #:sb-posix))
  :in-order-to ((asdf:test-op (asdf:test-op #:hypergeometrica/tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "logging")
               (:file "mmap-patches" :if-feature :linux)
               (:file "mmap")
               (:file "timing-utilities")
               (:file "math-utilities")
               (:file "sbcl-intrinsics"         :if-feature :sbcl)
               (:file "sbcl-intrinsics-x86-64"  :if-feature (:and :sbcl :x86-64))
               (:file "sbcl-intrinsics-ppc64el" :if-feature (:and :sbcl :ppc64 :little-endian))
               (:file "digit")
               (:file "vec")
               (:file "ram-vec")
               (:file "disk-vec")
               (:file "modular-arithmetic")
               (:file "mpz-protocol")
               (:file "mpz-ram")
               (:file "moduli")
               (:file "strandh-elster-reversal")
               (:file "number-theoretic-transform")
               (:file "ntt-multiply")
               ;; (:file "fft-multiply")
               ;; (:file "disk")
               (:file "multiply")
               (:file "mpd")
               (:file "divrem")
               (:file "mpz-string")
               (:file "binary-splitting")
               (:file "pi")))

(asdf:defsystem #:hypergeometrica/tests
  :description "Tests for HYPERGEOMETRICA."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :defsystem-depends-on (#:uiop)
  :depends-on (#:hypergeometrica
               #:fiasco

               (:feature :sbcl #:sb-mpfr))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:hypergeometrica-tests
                                           '#:test-hypergeometrica))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "arithmetic")
               (:file "moduli")
               (:file "vec")
               (:file "mpz")
               (:file "multiplication")
               (:file "ntt")
               (:file "primes")
               (:file "sundries")
               (:file "divrem")
               (:file "write-number")
               (:file "pi")))
