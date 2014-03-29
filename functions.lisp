;;;; functions.lisp

(in-package #:libsass)

(defun make-with-contex (context body new free)
  (let ((result (gensym "result")))
    `(let ((,context (,new)))
       (let ((,result (progn ,@body)))
         (,free ,context)
         ,result))))

;;; string context

(defcfun "sass_new_context" (:pointer sass-context))

(defcfun "sass_free_context" :void
  (context (:pointer sass-context)))

(defcfun "sass_compile" :int
  (context (:pointer sass-context)))

(defmacro with-context ((context) &body body)
  (make-with-contex context body 'sass-new-context 'sass-free-context))

;;; file context

(defcfun "sass_new_file_context" (:pointer sass-file-context))

(defcfun "sass_free_file_context" :void
  (context (:pointer sass-file-context)))

(defcfun "sass_compile_file" :int
  (context (:pointer sass-file-context)))

(defmacro with-file-context ((context) &body body)
  (make-with-contex context body 'sass-new-file-context 'sass-free-file-context))

;;; folder context

(defcfun "sass_new_folder_context" (:pointer sass-folder-context))

(defcfun "sass_free_folder_context" :void
  (context (:pointer sass-folder-context)))

(defcfun "sass_compile_folder" :int
  (context (:pointer sass-folder-context)))

(defmacro with-folder-context ((context) &body body)
  (make-with-contex context body 'sass-new-folder-context 'sass-free-folder-context))
