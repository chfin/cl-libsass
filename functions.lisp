;;;; functions.lisp

(in-package #:libsass)

(defun make-with-contex (context body new free)
  (let ((result (gensym "result")))
    `(let ((,context (,new)))
       (unwind-protect (progn ,@body)
         (,free ,context)))))

;;; string context

(defcfun "sass_new_context" (:pointer (:struct sass-context)))

(defcfun "sass_free_context" :void
  (context (:pointer (:struct sass-context))))

(defcfun "sass_compile" :int
  (context (:pointer (:struct sass-context))))

(defmacro with-context ((context) &body body)
  (make-with-contex context body 'sass-new-context 'sass-free-context))

;;; file context

(defcfun "sass_new_file_context" (:pointer (:struct sass-file-context)))

(defcfun "sass_free_file_context" :void
  (context (:pointer (:struct sass-file-context))))

(defcfun "sass_compile_file" :int
  (context (:pointer (:struct sass-file-context))))

(defmacro with-file-context ((context) &body body)
  (make-with-contex context body 'sass-new-file-context 'sass-free-file-context))

;;; folder context

(defcfun "sass_new_folder_context" (:pointer (:struct sass-folder-context)))

(defcfun "sass_free_folder_context" :void
  (context (:pointer (:struct sass-folder-context))))

(defcfun "sass_compile_folder" :int
  (context (:pointer (:struct sass-folder-context))))

(defmacro with-folder-context ((context) &body body)
  (make-with-contex context body 'sass-new-folder-context 'sass-free-folder-context))
