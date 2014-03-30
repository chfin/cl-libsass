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
(defvar *default-include-paths* nil
  "Default value for include paths `pathname` or `(list pathname)`")
(defvar *default-image-path* "images"
  "Default image path `string`")
(defvar *default-precision* 5
  "Default precision level `integer`")

(define-condition sass-error (error)
  ((status :initarg :status
           :reader sass-error-status)
   (message :initarg :message
            :reader sass-error-message)))

(defun set-options (options output-style source-comments include-paths image-path ;;precision
                    )
  (setf (foreign-slot-value options 'sass-options 'output-style)
        (getf *sass-style* output-style))
  (setf (foreign-slot-value options 'sass-options 'source-comments)
        (getf *sass-source-comments* source-comments))
  (setf (foreign-slot-value options 'sass-options 'include-paths)
        (format nil "~{~a~^:~}" (alexandria:ensure-list include-paths)))
  (setf (foreign-slot-value options 'sass-options 'image-path)
        (princ-to-string image-path))
  ;;(setf (foreign-slot-value options 'sass-options 'precision) precision) ;;for post 1.0.1
  )

(defun handle-errors (context type)
  (let ((error-status (foreign-slot-value context type 'error-status)))
    (unless (= error-status 0)
      (let ((error-message (foreign-slot-value context type 'error-message)))
        (error 'sass-error :status error-status :message error-message)))))

(defmacro define-sass-fun (name params docstring (type compile-fn context-macro result-expr)
                           &body body)
  `(defun ,name (,@params
                 &key (output-style *default-output-style*)
                   (source-comments *default-source-comments*)
                   (include-paths *default-include-paths*)
                   (image-path *default-image-path*)
                   ;;(precision *default-precision*) ;;for post 1.0.1
                   )
     ,docstring
     (restart-case
         (,context-macro (context)
           (set-options (foreign-slot-pointer context '(:struct ,type) 'options)
                        output-style source-comments include-paths image-path ;;precision
                        )
           ,@body
           (,compile-fn context)
           (handle-errors context '(:struct ,type))
           ,result-expr)
       (skip-compilation () nil))))

(define-sass-fun sass (in)
    "=> compiled CSS
Compiles `in` which is a Sass string."
    (sass-context sass-compile with-context
                  (foreign-slot-value context 'sass-context 'output-string))
  (setf (foreign-slot-value context 'sass-context 'source-string) in))

(define-sass-fun sass-file (input-path output-path)
    "=> compiled CSS
Compiles the file specified by `input-path` and saves it to `output-path`.
`input-path` and `output-path` are pathname designators."
    (sass-file-context sass-compile-file with-file-context
                       (alexandria:write-string-into-file
                        (foreign-slot-value context 'sass-file-context 'output-string)
                        output-path :if-exists :supersede))
  (setf (foreign-slot-value context 'sass-file-context 'input-path)
        (princ-to-string input-path))
  #|git: (setf (foreign-slot-value context 'sass-file-context 'output-path)
  (princ-to-string output-path))|#)

(define-sass-fun sass-folder (search-path output-path)
    "=> t
Compiles all Sass files from `search-path` to `output-path`.
`search-path` and `output-path` are pathname designators."
    (sass-folder-context sass-compile-folder with-folder-context t)
  (setf (foreign-slot-value context 'sass-folder-context 'search-path)
        (princ-to-string search-path))
  (setf (foreign-slot-value context 'sass-folder-context 'output-path)
        (princ-to-string output-path)))
