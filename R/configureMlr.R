#' @title Configures the behavior of the package.
#'
#' @description
#' Configuration is done by setting custom \code{\link{options}}.
#'
#' If you do not set an option here, its current value will be kept.
#'
#' If you call this function with an empty argument list, everything is set to its defaults.
#'
#' @param show.info [\code{logical(1)}]\cr
#'   Some methods of mlr support a \code{show.info} argument to enable
#'   verbose output on the console. This option sets the default value for these arguments.
#'   Setting the argument manually in one of these functions will overwrite the default
#'   value for that specific function call.
#'   Default is \code{TRUE}.
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
#' @param on.par.out.of.bounds [\code{character(1)}]\cr
#'   What should happen if a parameter of a learner is set to an out of bounds value.\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but parameter is still passed along to learner.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param show.learner.output [\code{logical(1)}]\cr
#'   Should the output of the learning algorithm during training and prediction be shown or captured and
#'   suppressed?
#'   Default is \code{TRUE}.
#' @template ret_inv_null
#' @family configure
#' @export
configureMlr = function(show.info, on.learner.error, on.learner.warning,
  on.par.without.desc, on.par.out.of.bounds, show.learner.output) {

  defaults = list(
    show.info = TRUE,
    on.learner.error = "stop",
    on.learner.warning = "warn",
    on.par.without.desc = "stop",
    on.par.out.of.bounds = "stop",
    show.learner.output = TRUE
  )

  any.change = FALSE
  if (!missing(show.info)) {
    assertFlag(show.info)
    setMlrOption("show.info", show.info)
    any.change = TRUE
  }
  if (!missing(on.learner.error)) {
    assertChoice(on.learner.error, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.learner.error", on.learner.error)
    any.change = TRUE
  }
  if (!missing(on.learner.warning)) {
    assertChoice(on.learner.warning, choices = c("warn", "quiet"))
    setMlrOption("on.learner.warning", on.learner.warning)
    any.change = TRUE
  }
  if (!missing(on.par.without.desc)) {
    assertChoice(on.par.without.desc, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.par.without.desc", on.par.without.desc)
    any.change = TRUE
  }
  if (!missing(on.par.out.of.bounds)) {
    assertChoice(on.par.out.of.bounds, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.par.out.of.bounds", on.par.out.of.bounds)
    any.change = TRUE
  }
  if (!missing(show.learner.output)) {
    assertFlag(show.learner.output)
    setMlrOption("show.learner.output", show.learner.output)
    any.change = TRUE
  }

  # no change, set everything to defaults
  # FIXME: this is a horrible mechanism! How can I get a list of all mlr options?
  if (!any.change)
    Map(setMlrOption, names(defaults), defaults)
  invisible(NULL)
}
