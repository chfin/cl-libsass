;;;; libsass.lisp

(in-package #:libsass)

(defparameter *sass-style*
  '(:nested 0
    :expanded 1
    :compact 2
    :compressed 3))

(defparameter *sass-source-comments*
  '(:none 0
    :default 1
    :map 2))

(defvar *default-output-style* :nested
  "Default output style `(or :nested :expanded :compact :compressed)`.")
(defvar *default-source-comments* :none
  "Default value for source comments `(or :none :default :map)`.")
(defvar *default-include-paths* ""
  "Default value for include paths `string`")
(defvar *default-image-path* "images"
  "Default image path `string`")
(defvar *default-precision* 5
  "Default precision level `integer`")

(define-condition sass-error (error)
  ((status :initarg :status
           :reader sass-error-status)
   (message :initarg :message
            :reader sass-error-message)))

(defun set-options (options output-style source-comments include-paths image-path precision)
  (setf (foreign-slot-value options 'sass-options 'output-style)
        (getf *sass-style* output-style))
  (setf (foreign-slot-value options 'sass-options 'source-comments)
        (getf *sass-source-comments* source-comments))
  (setf (foreign-slot-value options 'sass-options 'include-paths) include-paths)
  (setf (foreign-slot-value options 'sass-options 'image-path) image-path)
  (setf (foreign-slot-value options 'sass-options 'precision) precision))

(defun handle-errors (context type)
  (let ((error-status (foreign-slot-value context type 'error-status))
        (error-message (foreign-slot-value context type 'error-message)))
    (unless (= error-status 0)
          (error 'sass-error :status error-status :message error-message))))

(defmacro define-sass-fun (name params docstring (type compile-fn result-expr) &body body)
  `(defun ,name (,@params
                 &key (output-style *default-output-style*)
                   (source-comments *default-source-comments*)
                   (include-paths *default-include-paths*)
                   (image-path *default-image-path*)
                   (precision *default-precision*))
     ,docstring
     (restart-case
         (with-context (context)
           (set-options (foreign-slot-pointer context ',type 'options)
                        output-style source-comments include-paths image-path precision)
           ,@body
           (,compile-fn context)
           (handle-errors context ',type)
           ,result-expr)
       (skip-compilation () nil))))

(define-sass-fun sass (in)
    "=> compiled CSS
Compiles `in` which is a Sass string."
    (sass-context sass-compile (foreign-slot-value context 'sass-context 'output-string))
  (setf (foreign-slot-value context 'sass-context 'source-string) in))

(define-sass-fun sass-file (input-path output-path)
    "=> compiled CSS
Compiles the file specified by `input-path` and saves it to `output-path`.
`input-path` and `output-path` are pathname designators."
    (sass-file-context sass-compile-file
                       (foreign-slot-value context 'sass-file-context 'output-string))
  (setf (foreign-slot-value context 'sass-file-context 'input-path)
        (princ-to-string in-path))
  (setf (foreign-slot-value context 'sass-file-context 'output-path)
        (princ-to-string out-path)))

(define-sass-fun sass-folder (search-path output-path)
    "=> t
Compiles all Sass files from `search-path` to `output-path`.
`search-path` and `output-path` are pathname designators."
    (sass-folder-context sass-compile-folder t)
  (setf (foreign-slot-value context 'sass-folder-context 'search-path)
        (princ-to-string search-path))
  (setf (foreign-slot-value context 'sass-folder-context 'output-path)
        (princ-to-string output-path)))
