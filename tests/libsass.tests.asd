;;;; libsass.tests.asd

(asdf:defsystem #:libsass.tests
  :serial t
  :description "libsass tests"
  :author "Christoph Finkensiep"
  :mailto "chfin@freenet.de"
  :license "MIT/X11"
  :depends-on (#:libsass #:fiveam)
  :components ((:file "package")
               (:file "utils")
               (:file "tests")))
