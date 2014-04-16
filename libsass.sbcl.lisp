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

(defmacro check-error (error-var &body body)
  (alexandria:with-gensyms (ret msg status code)
    `(let (,ret ,msg)
       (setf ,msg
             (with-output-to-string (,error-var)
               (multiple-value-bind (,status ,code)
                   (progn ,@body)
                 (setf ,ret ,code))))
       (when (not (equal ,ret 0))
         (error 'sass-error :status ,ret :message ,msg)))))

(defun make-options (output-style line-comments source-map include-paths image-path)
  (declare (ignore image-path))
  (when (and line-comments source-map)
    (warn "line-comments and source-map are incompatible, preferring source-map"))
  `(,@(when (eq output-style :compressed)
            '("-t" "compressed"))
      ,@(when include-paths
              (list "-I"
                    (format nil "~{~a~^:~}" (alexandria:ensure-list include-paths))))
      ,@(when (and line-comments (not source-map))
              '("-l"))
      ,@(when source-map
              '("-g"))))

(defun sass (in &key (output-style *default-output-style*)
                  (line-comments *default-line-comments*)
                  (source-map *default-source-map*)
                  (include-paths *default-include-paths*)
                  (image-path *default-image-path*))
  "=> compiled CSS
Compiles `in` which is a Sass string."
  (let ((options (make-options output-style line-comments source-map include-paths image-path)))
    (with-input-from-string (sin in)
      (with-output-to-string (sout)
        (check-error serr
          (external-program:run "sassc" options :input sin :output sout :error serr))))))

(defun sass-file (input-path output-path
                  &key (output-style *default-output-style*)
                    (line-comments *default-line-comments*)
                    (source-map *default-source-map*)
                    (include-paths *default-include-paths*)
                    (image-path *default-image-path*))
  "=> compiled CSS
Compiles the file specified by `input-path` and saves it to `output-path`.
`input-path` and `output-path` are pathname designators."
  (let ((options (make-options output-style line-comments source-map include-paths image-path))
        (args (list "-o" (princ-to-string output-path) (princ-to-string input-path))))
    (check-error serr
      (external-program:run "sassc" (append options args) :error serr))
    (if source-map
        (values
         (alexandria:read-file-into-string output-path)
         (alexandria:read-file-into-string (format nil "~a.map" output-path))
         )
        (alexandria:read-file-into-string output-path))))
