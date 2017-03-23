# linters that differ from the default linters
# this is necessary because mlr's style is weird.
library("lintr")
library("rex")

# The following functions are adaptions of the corresponding functions in the `lintr` packages
# The lintr package, and the original versions of these functions, can be found at https://github.com/jimhester/lintr
# Copyright notice of original functions:
# Copyright (c) 2014-2016, James Hester
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# End copyright notice.
# All modifications are licensed as the rest of mlr.
 
# prohibit <-
left.assign.linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
      parsed = lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use =, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}

# prohibit ->
right.assign.linter = function(source_file) {
  lapply(lintr:::ids_with_token(source_file, "RIGHT_ASSIGN"), function(id) {
      parsed = lintr:::with_id(source_file, id)
      Lint(filename = source_file$filename, line_number = parsed$line1,
        column_number = parsed$col1, type = "style", message = "Use =, not =, for assignment.",
        line = source_file$lines[as.character(parsed$line1)],
        linter = "assignment_linter")
  })
}

`%!=%` = lintr:::`%!=%`
`%==%` = lintr:::`%==%`

spaces.left.parentheses.linter = function(source_file) {
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
            non_space_before = re_matches(before_operator, rex(non_space))
            not_exception = !(before_operator %in% c("!", ":",
                "[", "("))
            if (non_space_before && not_exception) {
                Lint(filename = source_file$filename, line_number = parsed$line1,
                  column_number = parsed$col1, type = "style",
                  message = "Place a space before left parenthesis, except in a function call.",
                  line = line, linter = "spaces.left.parentheses.linter")
            }
        }
    })
}

function.left.parentheses.linter = function(source_file) {
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

        space_before = re_matches(before_operator, rex(space))

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

infix.spaces.linter = function(source_file) {
    lapply(lintr:::ids_with_token(source_file, lintr:::infix_tokens, fun = `%in%`),
        function(id) {
            parsed = lintr:::with_id(source_file, id)
            line = source_file$lines[as.character(parsed$line1)]
            if (substr(line, parsed$col1, parsed$col2) == "^") {
              return(NULL)
            }
            around_operator = substr(line, parsed$col1 - 1L,
                parsed$col2 + 1L)
            non_space_before = re_matches(around_operator, rex(start,
                non_space))
            newline_after = unname(nchar(line)) %==% parsed$col2
            non_space_after = re_matches(around_operator, rex(non_space,
                end))
            if (non_space_before || (!newline_after && non_space_after)) {
                is_infix = length(lintr:::siblings(source_file$parsed_content,
                  parsed$id, 1)) > 1L
                start = end = parsed$col1
                if (is_infix) {
                  if (non_space_before) {
                    start = parsed$col1 - 1L
                  }
                  if (non_space_after) {
                    end = parsed$col2 + 1L
                  }
                  Lint(filename = source_file$filename, line_number = parsed$line1,
                    column_number = parsed$col1, type = "style",
                    message = "Put spaces around all infix operators.",
                    line = line, ranges = list(c(start, end)),
                    linter = "infix.spaces.linter")
                }
            }
        })
}


loweralnum = rex(one_of(lower, digit))
upperalnum = rex(one_of(upper, digit))

style.regexes = list(
  "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
  "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
  "snake_case"     = rex(start, one_or_more(loweralnum), zero_or_more("_", one_or_more(loweralnum)), end),
  "dotted.case"    = rex(start, one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
  "alllowercase"   = rex(start, one_or_more(loweralnum), end),
  "ALLUPPERCASE"   = rex(start, one_or_more(upperalnum), end),
  "functionCamel.case" = rex(start, lower, zero_or_more(alnum), zero_or_more(dot, one_or_more(alnum)), end)
)

# incorporate our own camelCase.withDots style.
matches_styles = function(name, styles=names(style.regexes)) {
  invalids = paste(styles[!styles %in% names(style.regexes)], collapse=", ")
  if (nzchar(invalids)) {
    valids = paste(names(style.regexes), collapse=", ")
    stop(sprintf("Invalid style(s) requested: %s\nValid styles are: %s\n", invalids, valids))
  }
  name = re_substitutes(name, rex(start, one_or_more(dot)), "")  # remove leading dots
  vapply(
    style.regexes[styles],
    re_matches,
    logical(1L),
    data=name
  )
}

object_naming_linter = lintr:::make_object_linter(function(source_file, token) {
  sp = source_file$parsed_content
  if (tail(c("", sp$token[sp$terminal & sp$id < token$id]), n = 1) == "'$'") {
    # ignore list member names
    return(NULL)
  }
  sp = head(sp[sp$terminal & sp$id > token$id, ], n = 2)
  if (!sp$token[1] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
    # ignore if not an assignment.
    # we check for LEFT_ASSIGN and EQ_ASSIGN since here we are LEFT_ASSIGN tolerant
    return(NULL)
  }
  style = ifelse(sp$token[2] == "FUNCTION", "functionCamel.case", "dotted.case")
  name = lintr:::unquote(token[["text"]])
  if (nchar(name) <= 1) {
    # allow single uppercase letter
    return(NULL)
  }
  if (!matches_styles(name, style)) {
    lintr:::object_lint(source_file, token, sprintf("Variable or function name should be %s.", 
      style), "object_name_linter")
  }
})


# note that this must be a *named* list (bug in lintr)
linters = list(
  commas = lintr:::commas_linter,
#  infix.spaces = infix.spaces.linter,
#  open.curly = open_curly_linter(),
#  closed.curly = closed_curly_linter(),
  spaces.left.parentheses = spaces.left.parentheses.linter,
  function.left.parentheses = function.left.parentheses.linter,
#   snake.case = snake_case_linter,
#   absolute.paths = absolute_paths_linter,
  single.quotes = lintr:::single_quotes_linter,
  left.assign = left.assign.linter,
  right.assign = right.assign.linter,
  no.tab = lintr:::no_tab_linter,
  T.and.F.symbol = lintr:::T_and_F_symbol_linter,
  semicolon.terminator = lintr:::semicolon_terminator_linter,
  seq = lintr:::seq_linter,
  unneeded.concatenation = lintr:::unneeded_concatenation_linter,
  trailing.whitespace = lintr:::trailing_whitespace_linter,
  todo.comment = lintr:::todo_comment_linter(todo = "todo"), # is case-insensitive
  spaces.inside = lintr:::spaces_inside_linter,
  infix.spaces = infix.spaces.linter,
  object.naming = object_naming_linter)

