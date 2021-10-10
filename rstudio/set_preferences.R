# install.packages("rstudio.prefs")
library(rstudio.prefs)

use_rstudio_prefs(
  # Saving settings
  load_workspace = FALSE,
  save_workspace = "never",
  always_save_history = FALSE,
  restore_last_project = FALSE,
  restore_source_documents = FALSE,
  restore_source_document_cursor_position = FALSE,

  # Personalization
  document_author = "Eric Leung",
  editor_theme = "Pastel On Dark",

  # Developer experience (DX)
  editor_keybindings = "vim",
  highlight_selected_line = TRUE,
  show_indent_guides = TRUE,
  warn_if_no_such_variable_in_scope = TRUE,
  warn_variable_defined_but_not_used = TRUE,
  style_diagnostics = TRUE,
  show_last_dot_value = TRUE,
  highlight_r_function_calls = TRUE,
  auto_append_newline = TRUE,
  strip_trailing_whitespace = TRUE,
  show_help_tooltip_on_idle = TRUE,
  check_arguments_to_r_function_calls = TRUE,
  check_unexpected_assignment_in_function_call = TRUE,
  show_diagnostics_other = TRUE,
  syntax_color_console = TRUE,

  # Layout UI
  visual_markdown_editing_show_margin = TRUE,
  show_rmd_render_command = TRUE,
  show_doc_outline_rmd = TRUE,
  jobs_tab_visibility = "shown",
  doc_outline_show = "sections_and_chunks",

  # Typesetting
  use_tinytex = TRUE,

  # Package development
  save_files_before_build = TRUE,
  view_dir_after_r_cmd_check = TRUE
)
