;;;; libsass.asd

(asdf:defsystem #:libsass
  :serial t
  :description "Describe libsass here"
  :author "Christoph Finkensiep"
  :mailto "chfin@freenet.de"
  :license "MIT/X11"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "library")
               (:file "types")
               (:file "functions")
               (:file "libsass")))

