#' @title Configures the behavior of the package.
#'
#' @description
#' Configuration is done by setting custom \code{\link{options}}.
#'
#' If you do not set an option here, its current value will be kept.
#'
#' If you call this function with an empty argument list, everything is set to its defaults.
#'
#' @param on.learner.error [\code{character(1)}]\cr
#'   What should happen if an error in an underlying learning algorithm is caught:\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: A \code{FailureModel} will be created, which predicts only NAs and a warning will be generated.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param on.learner.warning [\code{character(1)}]\cr
#'   What should happen if a warning in an underlying learning algorithm is generated:\cr
#'   \dQuote{warn}: The warning is generated as usual.\cr
#'   \dQuote{quiet}: The warning is suppressed.\cr
#'   Default is \dQuote{warn}.
#' @param on.par.without.desc [\code{character(1)}]\cr
#'   What should happen if a parameter of a learner is set to a value, but no parameter description object exists,
#'   indicating a possibly wrong name:\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but parameter is still passed along to learner.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param show.learner.output [\code{logical(1)}]\cr
#'   Should the output of the learning algorithm during training and prediction be shown or captured and
#'   suppressed?
#'   Default is \code{TRUE}.
#' @return Nothing.
#' @export
configureMlr = function(on.learner.error, on.learner.warning, on.par.without.desc, show.learner.output) {

  defaults = list(
    on.learner.error = "stop",
    on.learner.warning = "warn",
    on.par.without.desc = "stop",
    show.learner.output = TRUE
  )

  any.change = FALSE
  if (!missing(on.learner.error)) {
    checkArg(on.learner.error, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.learner.error", on.learner.error)
    any.change = TRUE
  }
  if (!missing(on.learner.warning)) {
    checkArg(on.learner.warning, choices = c("warn", "quiet"))
    setMlrOption("on.learner.warning", on.learner.warning)
    any.change = TRUE
  }
  if (!missing(on.par.without.desc)) {
    checkArg(on.par.without.desc, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.par.without.desc", on.par.without.desc)
    any.change = TRUE
  }
  if (!missing(show.learner.output)) {
    checkArg(show.learner.output, "logical", len = 1L, na.ok = FALSE)
    setMlrOption("show.learner.output", show.learner.output)
    any.change = TRUE
  }

  # na change, set everything to defaults
  if (!any.change)
    Map(setMlrOption, names(defaults), defaults)
  invisible(NULL)
}
