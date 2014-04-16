;;;; libsass.asd

(asdf:defsystem #:libsass
  :serial t
  :description "Common Lisp bindings to libsass"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:cffi #:alexandria #+sbcl #:external-program)
  :components ((:file "package")
               #-sbcl (:file "library")
               #-sbcl (:file "types")
               #-sbcl (:file "functions")
               #-sbcl (:file "libsass")
               #+sbcl (:file "libsass.sbcl")))

