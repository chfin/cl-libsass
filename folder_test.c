#include <stdio.h>
#include <sass_interface.h>

int main(void) {
  struct sass_options options;
  options.output_style = SASS_STYLE_NESTED;
  options.source_comments = 0;
  options.image_path = "images";
  options.include_paths = "";

  struct sass_folder_context* context = sass_new_folder_context ();
  context->search_path = "./test/files/folder_sass/";
  context->output_path = "./test/files/folder_css/";
  sass_compile_folder (context);
}
