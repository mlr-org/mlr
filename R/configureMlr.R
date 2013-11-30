#' Configures the behaviour of the package.
#'
#' Configuration is done by setting custom \code{\link{options}}.
#'
#' @param on.learner.error [\code{character(1)}]\cr
#'   What should happen if an error in an underlying learning algorithm is caught:\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: A \code{FailureModel} will be created, which predicts only NAs and a warning will be generated.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
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
configureMlr = function(on.learner.error="stop", on.par.without.desc="stop", show.learner.output=TRUE) {
  checkArg(on.learner.error, choices=c("quiet", "warn", "stop"))
  checkArg(on.par.without.desc, choices= c("quiet", "warn", "stop"))
  checkArg(show.learner.output, "logical", len=1L, na.ok=FALSE)
  setMlrOption("on.learner.error", on.learner.error)
  setMlrOption("on.par.without.desc", on.par.without.desc)
  setMlrOption("show.learner.output", show.learner.output)
  invisible(NULL)
}
