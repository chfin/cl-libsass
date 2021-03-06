;;;; libsass.lisp

(in-package #:libsass)

(defparameter *sass-style*
  '(:nested 0
    :expanded 1
    :compact 2
    :compressed 3))

(defvar *default-output-style* :nested
  "Default output style `(or :nested :expanded :compact :compressed)`")
(defvar *default-source-map* nil
  "Default value of the source-map option `boolean`")
(defvar *default-line-comments* nil
  "Default value of the line-comments option `boolean`")
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

(defmethod print-object ((object sass-error) stream)
  (print-unreadable-object (object stream)
    (format stream "An error (~a) occured in libsass: ~a"
            (sass-error-status object)
            (sass-error-message object))))

(defun set-options (options output-style line-comments source-map include-paths image-path
                    ;;precision
                    )
  (when (and line-comments source-map)
    (warn "line-comments and source-map are incompatible, preferring source-map"))
  (setf (foreign-slot-value options 'sass-options 'output-style)
        (getf *sass-style* output-style))
  (setf (foreign-slot-value options 'sass-options 'source-comments)
        (cond ;; comment style
          (source-map 2)
          (line-comments 1)
          (t 0)))
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
                   (line-comments *default-line-comments*)
                   (source-map *default-source-map*)
                   (include-paths *default-include-paths*)
                   (image-path *default-image-path*)
                   ;;(precision *default-precision*) ;;for post 1.0.1
                   )
     ,docstring
     (restart-case
         (,context-macro (context)
           (set-options (foreign-slot-pointer context '(:struct ,type) 'options)
                        output-style line-comments source-map include-paths image-path
                        ;;precision
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
                       (values
                        (alexandria:write-string-into-file
                         (foreign-slot-value context 'sass-file-context 'output-string)
                         output-path :if-exists :supersede)
                        (foreign-slot-value context 'sass-file-context 'source-map-string)))
  (setf (foreign-slot-value context 'sass-file-context 'input-path)
        (princ-to-string input-path))
  (when source-map
    (setf (foreign-slot-value context 'sass-file-context 'source-map-file)
          (format nil "~a.map" output-path)))
  #|git: (setf (foreign-slot-value context 'sass-file-context 'output-path)
  (princ-to-string output-path))|#)

(define-sass-fun sass-folder (search-path output-path)
    "=> t
Compiles all Sass files from `search-path` to `output-path`.
`search-path` and `output-path` are pathname designators."
    (sass-folder-context sass-compile-folder with-folder-context
                         (print (convert-from-foreign context '(:struct sass-folder-context))))
  (warn "libsass currently does not implement sass_compile_folder, so this is a stub.")
  (setf (foreign-slot-value context 'sass-folder-context 'search-path)
        (princ-to-string search-path))
  (setf (foreign-slot-value context 'sass-folder-context 'output-path)
        (princ-to-string output-path)))
