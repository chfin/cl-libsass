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
    (do-dir (path (rel-path "files/*.scss"))
      (sass-file path (css-path path))
      (is-true (probe-file (css-path path)))
      (is (file-equal (css-path path) (cmp-path (css-path path)))))))

;;; sass-folder

(test sass-folder
  "Tests the folder based sass function"
  (with-fixture clean-foldes ((rel-path "files/folder_css/*.css"))
    (sass-folder (rel-path "files/folder_sass/")
                 (rel-path "files/folder_css/"))
    (do-dir (path (rel-path "files/folder_sass/*.scss"))
      (is-true (probe-file (css-path path))))
    (do-dir (path (rel-path "files/folder_css/*.css"))
      (is (file-equal path (cmp-path path))))))

(defun run-tests ()
  (run! 'sass-tests))

(defun run-sass-file ()
  (sass-file (rel-path "files/test_correct.scss")
             (rel-path "files/test_correct.css")))