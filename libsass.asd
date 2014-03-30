;;;; libsass.asd

(asdf:defsystem #:libsass
  :serial t
  :description "Common Lisp bindings to libsass"
  :author "Christoph Finkensiep"
  :mailto "chfin@freenet.de"
  :license "MIT/X11"
  :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "library")
               (:file "types")
               (:file "functions")
               (:file "libsass")))

