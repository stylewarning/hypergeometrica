;;;; hypergeometrica-manager.asd
;;;;
;;;; Copyright (c) 2024 Robert Smith

(asdf:defsystem #:hypergeometrica-manager
  :description "Manager process for running large Hypergeometrica calculations."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:clingon #:uiop #:bordeaux-threads #:sb-bsd-sockets)
;  :in-order-to ((asdf:test-op (asdf:test-op #:hypergeometrica-manager/tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src-manager/"
  :serial t
  :components ((:file "package")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "hypergeometrica-manager"
  :entry-point "hypergeometrica-manager:main")

(asdf:defsystem #:hypergeometrica-manager/tests
  :description "Tests for HYPERGEOMETRICA-MANAGER."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :defsystem-depends-on (#:uiop)
  :depends-on (#:hypergeometrica-manager
               #:fiasco)
  :perform (asdf:test-op (o s)
                         #+ignore
                         (uiop:symbol-call '#:hypergeometrica-tests
                                           '#:test-hypergeometrica))
  :serial t
  :components ())
