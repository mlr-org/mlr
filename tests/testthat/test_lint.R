if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("lint check", {
    # note that this must be a *named* list (bug in lintr)
    linters = list(
      commas = lintr::commas_linter,
      infix.spaces = lintr::infix_spaces_linter,
      open.curly = lintr::open_curly_linter(),
      closed.curly = lintr::closed_curly_linter(),
      spaces.left.parentheses = lintr::spaces_left_parentheses_linter,
      snake.case = lintr::snake_case_linter,
      absolute.paths = lintr::absolute_paths_linter,
      single.quotes = lintr::single_quotes_linter)
    lintr::expect_lint_free(linters = linters)
  })
}

