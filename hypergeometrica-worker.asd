;;;; hypergeometrica-worker.asd
;;;;
;;;; Copyright (c) 2024 Robert Smith

(asdf:defsystem #:hypergeometrica-worker
  :description "Worker process for running large Hypergeometrica calculations."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:clingon #:uiop #:bordeaux-threads #:sb-bsd-sockets)
;  :in-order-to ((asdf:test-op (asdf:test-op #:hypergeometrica-worker/tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src-worker/"
  :serial t
  :components ((:file "package")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "hypergeometrica-worker"
  :entry-point "hypergeometrica-worker:main")

(asdf:defsystem #:hypergeometrica-worker/tests
  :description "Tests for HYPERGEOMETRICA-WORKER."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :defsystem-depends-on (#:uiop)
  :depends-on (#:hypergeometrica-worker
               #:fiasco)
  :perform (asdf:test-op (o s)
                         #+ignore
                         (uiop:symbol-call '#:hypergeometrica-tests
                                           '#:test-hypergeometrica))
  :serial t
  :components ())
