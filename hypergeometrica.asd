;;;; hypergeometrica.asd
;;;;
;;;; Copyright (c) 2019 Robert Smith

(asdf:defsystem #:hypergeometrica
  :description "Calculate lots of digits of things."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:alexandria
               #:global-vars
               #:lparallel
               #:napa-fft3
               #:trivial-garbage
               #:uiop)
  :in-order-to ((asdf:test-op (asdf:test-op #:hypergeometrica/tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "math-utilities")
               (:file "sbcl-intrinsics"         :if-feature :sbcl)
               (:file "sbcl-intrinsics-x86-64"  :if-feature (:and :sbcl :x86-64))
               (:file "sbcl-intrinsics-ppc64el" :if-feature (:and :sbcl :ppc64 :little-endian))
               (:file "digit")
               (:file "modular-arithmetic")
               (:file "storage")
               (:file "mpz")
               (:file "moduli")
               (:file "strandh-elster-reversal")
               (:file "number-theoretic-transform")
               (:file "ntt-multiply")
               (:file "fft-multiply")
               (:file "disk")
               (:file "multiply")
               (:file "binary-splitting")))

(asdf:defsystem #:hypergeometrica/tests
  :description "Tests for HYPERGEOMETRICA."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :defsystem-depends-on (#:uiop)
  :depends-on (#:hypergeometrica
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:hypergeometrica-tests
                                           '#:test-hypergeometrica))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "arithmetic")
               (:file "moduli")
               (:file "mpz")
               (:file "multiplication")
               (:file "ntt")
               (:file "primes")
               (:file "sundries")))
