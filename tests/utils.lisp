;;;; utils.lisp

(in-package #:libsass.tests)

(defparameter *system-location*
  (asdf:system-source-directory "libsass.tests"))

(defun rel-path (path)
  (merge-pathnames path *system-location*))

(defun file-equal (path1 path2)
  (equal (alexandria:read-file-into-string path1)
         (alexandria:read-file-into-string path2)))

(defmacro do-dir ((var path) &body body)
  `(dolist (,var (directory ,path))
     ,@body))

(defun cmp-path (path)
  (pathname (concatenate 'string (princ-to-string path) ".cmp")))

(defun css-path (path)
  (make-pathname :type "css" :defaults path))
