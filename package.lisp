;;;; package.lisp

(defpackage #:libsass
  (:use #:cl #:cffi #+sbcl #:sb-alien)
  (:export #:sass #:sass-file ;;#:sass-folder
           #:*default-output-style* #:*default-source-comments*
           #:*default-include-paths* #:*default-image-path*
           #:*default-precision* #:sass-error))
