#' Convert arguments to control structure.
#'
#' Find all elements in \code{...} which are not missing and
#' call \code{control} on them.
#'
#' @param control [\code{function}]\cr
#'   Function that creates control structure.
#' @param ... [any]\cr
#'   Arguments for control structure function.
#' @return Control structure for learner.
#' @export
learnerArgsToControl = function(control, ...) {
  args = list()
  dots = match.call(expand.dots = FALSE)$...
  print(str(dots))
  for (i in seq_along(dots)) {
    arg = dots[[i]]
    is_missing = if (is.symbol(arg)) {
      argname = as.character(arg)
      eval(substitute(missing(symbol), list(symbol = arg)),
           envir = parent.frame())
    } else {
      argname = names(dots)[i]
      FALSE
    }
    if (!is_missing) {
      value = tryCatch(eval(arg, envir = parent.frame()),
                       error = function(...) NULL)
      # messagef("argname = %s", argname)
      # print("value:")
      # print(value)
      if (!is.null(value)) {
        args[[as.character(arg)]] = value
      }
    }
  }
  do.call(control, args)
}
