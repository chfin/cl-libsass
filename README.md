# cl-libsass

cl-libsass is a wrapper for [libsass](http://libsass.org/) (v1.0.1) using CFFI.

## Installation

Currently not on Quicklisp, but ASDF loadable.
If you are using SBCL you will need `sassc`.

## Usage

The package `libsass` exports three functions:

* `(sass in &key ...)`
  takes a string, compiles it and returns the resulting CSS as a string.
* `(sass-file input-path output-path &key ...)`
  compiles a file (`input-path`) and saves it to `output-path`.
  The compiled CSS is also returned as a string.
<!--
* `(sass-folder search-path output-path &key ...)`
  compiles all Sass files under `search-path` and saves them under `output-path`
  (**currently not implemented in libsass**).
-->

### Options

Each of these functions takes keyword arguments according to the options struct of libsass:

* `:output-style`
  is one of `:nested`, `:expanded`, `:compact` or `:compressed`.
* `:line-comments`
  is a boolean.
* `:source-map`
  is a boolean.
* `:include-paths`
  is a pathname designator or a list of pathname designators.
* `:image-path`
  is a pathname-designator
* `:precision`
  is an integer (currently not available because of libsass version 1.0.1)

If both `:line-comments` and `:source-map` are `t`,
a warning will be signaled and `:source-map` will be preferred,
since these optiont are incompatible in libsass.

The default values for these options are defined by special variables
which are also exported by the `libsass` package:

```common-lisp
(defvar *default-output-style* :nested)
(defvar *default-source-map* nil)
(defvar *default-line-comments* nil)
(defvar *default-include-paths* "")
(defvar *default-image-path* "images")
(defvar *default-precision* 5)
```

For more documentation refer to [libsass](http://libsass.org/).

## Tests

To run regression tests, load the system `libsass.tests` and call

```common-lisp
(libsass.tests:run-tests)
```

The test `sass-folder` will fail at the moment
as `sass_folder_context` is not fully implemented yet.

## License

X11/MIT
