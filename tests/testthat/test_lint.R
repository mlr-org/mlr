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

function_left_parentheses_linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "'('"),
    function(id) {

      parsed = source_file$parsed_content[id, ]
      ttb = which(source_file$parsed_content$line1 == parsed$line1 &
                  source_file$parsed_content$col1 < parsed$col1 &
                  source_file$parsed_content$terminal)
      ttb = tail(ttb, n = 1)
      last_type = source_file$parsed_content$token[ttb]

      is_function = length(last_type) %!=% 0L &&
        (last_type %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "')'", "']'"))
      # check whether this is a lambda expression; we want to allow e.g. function(x) (x - 1)^2
      if (is_function && last_type == "')'") {
        # parenvec: 1 for every '(', -1 for every ')', 0 otherwise
        parenvec = c(1, -1, 0)[match(source_file$parsed_content$token, c("'('", "')'"), 3)]
        parenlevel = cumsum(parenvec)
        parenlevelcut = parenlevel[seq_len(ttb - 1)]
        opening.paren.pos = max(which(parenlevelcut == parenlevel[ttb])) + 1
        opparsed = source_file$parsed_content[opening.paren.pos, ]

        opttb = which(source_file$parsed_content$line1 == opparsed$line1 &
                      source_file$parsed_content$col1 < opparsed$col1 &
                      source_file$parsed_content$terminal)
        opttb = tail(opttb, n = 1)
        before_op_type = source_file$parsed_content$token[opttb]
        if (length(before_op_type) %!=% 0L && before_op_type == "FUNCTION") {
          is_function = FALSE
        }
      }
      if (is_function) {

        line = source_file$lines[as.character(parsed$line1)]

        before_operator = substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

        space_before = rex::re_matches(before_operator, rex::rex(space))

        if (space_before) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = "Remove spaces before the left parenthesis in a function call.",
            line = line,
            linter = "function_left_parentheses"
            )
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
    function.left.parentheses = function_left_parentheses_linter,
#      snake.case = snake_case_linter,
#      absolute.paths = absolute_paths_linter,
    single.quotes = single_quotes_linter,
    left.assign = left_assign_linter,
    right.assign = right_assign_linter,
    no.tab = no_tab_linter,
    T.and.F.symbol = T_and_F_symbol_linter,
    semicolon.terminator = semicolon_terminator_linter,
    seq = seq_linter,
    unneeded.concatenation = unneeded_concatenation_linter,
    trailing.whitespace = trailing_whitespace_linter,
    todo.comment = todo_comment_linter(todo = "todo")) # is case-insensitive
  expect_lint_free(linters = linters)
})


