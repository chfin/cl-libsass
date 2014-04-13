;;;; tests.lisp

(in-package #:libsass.tests)

;;; definitions

(defparameter *example-scss*
  "$font-stack:    Helvetica, sans-serif;
$primary-color: #333;

body {
  font: 100% $font-stack;
  color: $primary-color;
}")

(defparameter *example-css*
  "body {
  font: 100% Helvetica, sans-serif;
  color: #333; }
")

(def-suite sass-tests
    :description "Test suite for sass tests")

(in-suite sass-tests)

(def-fixture clean-foldes (&rest folders)
  (dolist (folder folders)
    (do-dir (path folder)
      (when (probe-file path)
        (delete-file path))))
  (&body))

;;; sass

(test sass
  "Test the string in string out sass function"
  (is (equal (sass *example-scss*) *example-css*))
  (signals libsass:sass-error
    (sass "{")))

;;; sass-file

(test sass-file
  "Tests the file in file out sass function"
  (with-fixture clean-foldes ((rel-path "files/*.css"))
    (let ((correct (rel-path "files/correct.scss"))
          (fail (rel-path "files/fail.scss"))
          (include (rel-path "files/include.scss"))
          (line-comments (rel-path "files/line-comments.scss"))
          (source-map (rel-path "files/source-map.scss")))
      (sass-file correct (css-path correct))
      (is-true (probe-file (css-path correct)))
      (is (file-equal (css-path correct) (cmp-path correct)))
      
      (signals libsass:sass-error
        (sass-file fail (css-path fail)))
      
      (sass-file include (css-path include) :include-paths
                 (list (rel-path "files/include") (rel-path "files/include2")))
      (is-true (probe-file (css-path include)))
      (is (file-equal (css-path include) (cmp-path include)))
      
      (sass-file line-comments (css-path line-comments) :line-comments t)
      (is-true (probe-file (css-path line-comments)))
      (is (file-equal (css-path line-comments) (cmp-path line-comments)))
      
      (multiple-value-bind (result source-map-string)
          (sass-file source-map (css-path source-map) :source-map t)
        (is (equal result (alexandria:read-file-into-string
                           (rel-path "files/source-map.cmp"))))
        (is (equal source-map-string (alexandria:read-file-into-string
                                      (rel-path "files/source-map.cmp.map")))))
      (is-true (probe-file (css-path source-map)))
      (is (file-equal (css-path source-map) (cmp-path source-map)))
      
      (signals warning
        (sass-file source-map (css-path source-map) :source-map t :line-comments t))
      (is-true (probe-file (css-path source-map)))
      (is (file-equal (css-path source-map) (cmp-path source-map))))))

;;; sass-folder

(test sass-folder
  "Tests the folder based sass function"
  (let ((sass-dir (rel-path "files/folder_sass"))
        (css-dir (rel-path "files/folder_css")))
    (with-fixture clean-foldes ((rel-path "files/folder_css/*.css"))
      (signals warning
        (sass-folder sass-dir css-dir))
      #+nil(do-dir (path (rel-path "files/folder_sass/*.scss"))
        (is-true (probe-file (folder-path css-dir path))
                 "file does not exist: ~a (currently not implemented)"
                 (folder-path css-dir path)))
      #+nil(do-dir (path (rel-path "files/folder_css/*.css"))
        (is (file-equal path (cmp-path path)))))))

(test sass-sbcl-bug
  "Tests a strange bug on sbcl"
  (is (equal (sass "$foo: 40.063em; .test { fontsize: $foo; }")
             ".test {
  font-size: 40.063em; }
")))

(defun run-tests ()
  (run! 'sass-tests))

(defun run-sass-file ()
  (sass-file (rel-path "files/test_correct.scss")
             (rel-path "files/test_correct.css")))
