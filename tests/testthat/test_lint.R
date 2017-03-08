library("lintr")
context("lint")

# prohibit <-
left_assign_linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
      parsed <- lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use <-, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}

# prohibit ->
right_assign_linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "RIGHT_ASSIGN"), function(id) {
      parsed <- lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use <-, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}


test_that("lint check", {
  # note that this must be a *named* list (bug in lintr)
  linters = list(
    commas = commas_linter,
#    infix.spaces = infix_spaces_linter,
    open.curly = open_curly_linter(),
    closed.curly = closed_curly_linter(),
    spaces.left.parentheses = spaces_left_parentheses_linter,
#      snake.case = snake_case_linter,
#      absolute.paths = absolute_paths_linter,
    single.quotes = single_quotes_linter,
    left.assign = left_assign_linter,
    right.assign = right_assign_linter)
  expect_lint_free(linters = linters)
})


