#' Convert arguments to control structure.
#'
#' Find all elements in `...` which are not missing and
#' call `control` on them.
#'
#' @param control (`function`)\cr
#'   Function that creates control structure.
#' @param ... (any)\cr
#'   Arguments for control structure function.
#' @return Control structure for learner.
#' @export
learnerArgsToControl = function(control, ...) {
  args = list()
  dots = match.call(expand.dots = FALSE)$...
  for (i in seq_along(dots)) {
    arg = dots[[i]]
    is.missing = if (is.symbol(arg)) {
      argname = as.character(arg)
      eval(substitute(missing(symbol), list(symbol = arg)),
        envir = parent.frame())
    } else {
      argname = names(dots)[i]
      FALSE
    }
    if (!is.missing) {
      value = tryCatch(eval(arg, envir = parent.frame()),
        error = function(...) NULL)
      if (!is.null(value)) {
        args[[as.character(argname)]] = value
      }
    }
  }
  do.call(control, args)
}
