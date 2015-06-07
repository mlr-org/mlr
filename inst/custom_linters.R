#Custom set of linters for this project
#Need the github version of lintr
library(lintr)
my_linters <- list(
  assignment_linter=assignment_linter,
  single_quotes_linter=single_quotes_linter,
  absolute_paths_linter=absolute_paths_linter,
  no_tab_linter=no_tab_linter,
  commas_linter=commas_linter,
  spaces_left_parentheses_linter=spaces_left_parentheses_linter,
  spaces_inside_linter=spaces_inside_linter,
  open_curly_linter=open_curly_linter,
  closed_curly_linter=closed_curly_linter,
  multiple_dots_linter=multiple_dots_linter,
  object_length_linter=object_length_linter,
  object_usage_linter=object_usage_linter,
  trailing_whitespace_linter=trailing_whitespace_linter,
  trailing_blank_lines_linter=trailing_blank_lines_linter
)
devtools::use_data(my_linters, overwrite=TRUE)
