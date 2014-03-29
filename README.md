# cl-libsass

cl-libsass is a wrapper for [libsass](http://libsass.org/) using CFFI.

## Installation

Currently not on Quicklisp, but ASDF loadable.

## Usage

The package `libsass` exports three functions:

* `(sass in &key ...)`
  takes a string, compiles it and returns the resulting CSS as a string.
* `(sass-file input-path output-path &key ...)`
  compiles a file (`input-path`) and saves it to `output-path`.
  The compiled CSS is also returned as a string.
* `(sass-folder search-path output-path &key ...)`
  compiles all Sass files under `search-path` and saves them under `output-path`.

### Options

Each of these functions takes keyword arguments according to the options struct of libsass:

* `:output-style`
  is one of `:nested`, `:expanded`, `:compact` or `:compressed`.
* `:source-comments`
  is one of `:none`, `:default` or `:map`
* `:include-paths`
  is a string
* `:image-path`
  is a string
* `:precision`
  is an integer

The default values for these options are defined by special variables
which are also exported by the `libsass` package:

```common-lisp
(defvar *default-output-style* :nested)
(defvar *default-source-comments* :none)
(defvar *default-include-paths* "")
(defvar *default-image-path* "images")
(defvar *default-precision* 5)
```

For more documentation refer to [libsass](http://libsass.org/).

## License

X11/MIT