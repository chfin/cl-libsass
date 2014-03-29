;;;; types.lisp

(in-package #:libsass)

(defcstruct sass-options
  (output-style :int)
  (source-comments :int)
  (include-paths :string)
  (image-path :string)
  (precision :int))

(defcstruct sass-context
  (source-string :string)
  (output-string :string)
  (options (:struct sass-options))
  (error-status :int)
  (error-message :string)
  (c-functions :pointer)
  (included-files (:pointer :string))
  (num-included-files :int))

#|struct sass_context {
  const char* source_string;
  char* output_string;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;
};|#

(defcstruct sass-file-context
  (input-path :string)
  (output-path :string)
  (output-string :string)
  (source-map-string :string)
  (source-map-file :string)
  (options (:struct sass-options))
  (error-status :int)
  (error-message :string)
  (c-functions :pointer)
  (included-files (:pointer :string))
  (num-included-files :int))

#|struct sass_file_context {
  const char* input_path;
  const char* output_path;
  char* output_string;
  char* source_map_string;
  const char* source_map_file;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;
};|#

(defcstruct sass-folder-context
  (search-path :string)
  (output-path :string)
  (options (:struct sass-options))
  (error-status :int)
  (error-message :string)
  (c-functions :pointer)
  (included-files (:pointer :string))
  (num-included-files :int))

#|struct sass_folder_context {
  const char* search_path;
  const char* output_path;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;
};|#
