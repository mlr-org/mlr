library("lintr")
context("lint")

# prohibit <-
left_assign_linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
      parsed = lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use =, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}

# prohibit ->
right_assign_linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "RIGHT_ASSIGN"), function(id) {
      parsed = lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use =, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}

`%!=%` = lintr:::`%!=%`


spaces_left_parentheses_linter = function(source_file) {
      lapply(lintr:::ids_with_token(source_file, "'('"), function(id) {
        parsed = source_file$parsed_content[id, ]
        terminal_tokens_before = source_file$parsed_content$token[source_file$parsed_content$line1 == 
            parsed$line1 & source_file$parsed_content$col1 < 
            parsed$col1 & source_file$parsed_content$terminal]
        last_type = tail(terminal_tokens_before, n = 1)
        is_function = length(last_type) %!=% 0L && (last_type %in% 
            c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "')'", 
                "']'"))
        if (!is_function) {
            line = source_file$lines[as.character(parsed$line1)]
            before_operator = substr(line, parsed$col1 - 1L, 
                parsed$col1 - 1L)
            non_space_before = rex::re_matches(before_operator, rex::rex(non_space))
            not_exception = !(before_operator %in% c("!", ":", 
                "[", "("))
            if (non_space_before && not_exception) {
                Lint(filename = source_file$filename, line_number = parsed$line1, 
                  column_number = parsed$col1, type = "style", 
                  message = "Place a space before left parenthesis, except in a function call.", 
                  line = line, linter = "spaces_left_parentheses_linter")
            }
        }
    })
}


test_that("lint check", {
  # note that this must be a *named* list (bug in lintr)
  linters = list(
    commas = commas_linter,
#    infix.spaces = infix_spaces_linter,
#    open.curly = open_curly_linter(),
#    closed.curly = closed_curly_linter(),
    spaces.left.parentheses = spaces_left_parentheses_linter,
#      snake.case = snake_case_linter,
#      absolute.paths = absolute_paths_linter,
    single.quotes = single_quotes_linter,
    left.assign = left_assign_linter,
    right.assign = right_assign_linter)
  expect_lint_free(linters = linters)
})


