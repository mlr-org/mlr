#' @title Convert arguments to control structure.
#'
#' @description
#' Find all elements in \code{...} which are not missing and
#' call \code{control} on them.
#'
#' @param control [\code{function}]\cr
#'   Function that creates control structure.
#' @param ... [any]\cr
#'   Arguments for control structure function.
#' @param .defaults [\code{list}]\cr
#'   Initial default values that the control function is called on.
#'   One can use this if control should be called on different defaults than in its signature.
#'   The values in ... will overwrite these.
#'   Default is empty list.
#' @param .restrict [\code{logical(1)]\cr
#'   Further pick out only arguments from ... which occur in the signature of \code{control}?
#'   Default is \code{FALSE}.
#' @return Control structure for learner.
#' @export
learnerArgsToControl = function(control, ..., .defaults = list(), .restrict = FALSE) {
  if (.restrict)
    allowed.arg.names = names(formals(control))
  args = .defaults
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
    if (!is.missing && (!.restrict || argname %in% allowed.arg.names)) {
      value = tryCatch(eval(arg, envir = parent.frame()), error = function(...) NULL)
      if (!is.null(value)) {
        args[[as.character(argname)]] = value
      }
    }
  }
  do.call(control, args)
}
