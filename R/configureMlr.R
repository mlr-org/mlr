#' @title Configures the behavior of the package.
#'
#' @description
#' Configuration is done by setting custom [options].
#'
#' If you do not set an option here, its current value will be kept.
#'
#' If you call this function with an empty argument list, everything is set to its defaults.
#'
#' @param show.info (`logical(1)`)\cr
#'   Some methods of mlr support a `show.info` argument to enable
#'   verbose output on the console. This option sets the default value for these arguments.
#'   Setting the argument manually in one of these functions will overwrite the default
#'   value for that specific function call.
#'   Default is `TRUE`.
#' @param on.learner.error (`character(1)`)\cr
#'   What should happen if an error in an underlying learning algorithm is caught:\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: A `FailureModel` will be created, which predicts only NAs and a warning will be generated.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param on.learner.warning (`character(1)`)\cr
#'   What should happen if a warning in an underlying learning algorithm is generated:\cr
#'   \dQuote{warn}: The warning is generated as usual.\cr
#'   \dQuote{quiet}: The warning is suppressed.\cr
#'   Default is \dQuote{warn}.
#' @param on.par.without.desc (`character(1)`)\cr
#'   What should happen if a parameter of a learner is set to a value, but no parameter description object exists,
#'   indicating a possibly wrong name:\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but parameter is still passed along to learner.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param on.par.out.of.bounds (`character(1)`)\cr
#'   What should happen if a parameter of a learner is set to an out of bounds value.\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but parameter is still passed along to learner.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param on.measure.not.applicable (`logical(1)`)\cr
#'   What should happen if a measure is not applicable to a learner.\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but value of the measure will be `NA`.\cr
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{stop}.
#' @param show.learner.output (`logical(1)`)\cr
#'   Should the output of the learning algorithm during training and prediction be shown or captured and
#'   suppressed?
#'   Default is `TRUE`.
#' @param on.error.dump (`logical(1)`)\cr
#'   Specify whether [FailureModel] models and failed predictions should contain an error dump
#'   that can be used with `debugger` to inspect an error. This option is only effective if `on.learner.error`
#'   is \dQuote{warn} or \dQuote{quiet}. If it is `TRUE`, the dump can be accessed using
#'   [getFailureModelDump] on the [FailureModel], [getPredictionDump] on the failed prediction, and [getRRDump] on resample predictions.
#'   Default is `FALSE`.
#' @template ret_inv_null
#' @family configure
#' @export
configureMlr = function(show.info, on.learner.error, on.learner.warning,
  on.par.without.desc, on.par.out.of.bounds, on.measure.not.applicable,
  show.learner.output, on.error.dump) {

  defaults = list(
    show.info = TRUE,
    on.learner.error = "stop",
    on.learner.warning = "warn",
    on.par.without.desc = "stop",
    on.par.out.of.bounds = "stop",
    on.measure.not.applicable = "stop",
    show.learner.output = TRUE,
    on.error.dump = FALSE
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
  if (!missing(on.measure.not.applicable)) {
    assertChoice(on.measure.not.applicable, choices = c("quiet", "warn", "stop"))
    setMlrOption("on.measure.not.applicable", on.measure.not.applicable)
    any.change = TRUE
  }
  if (!missing(show.learner.output)) {
    assertFlag(show.learner.output)
    setMlrOption("show.learner.output", show.learner.output)
    any.change = TRUE
  }
  if (!missing(on.error.dump)) {
    assertFlag(on.error.dump)
    setMlrOption("on.error.dump", on.error.dump)
    any.change = TRUE
  }

  # no change, set everything to defaults
  # FIXME: this is a horrible mechanism! How can I get a list of all mlr options?
  if (!any.change) {
    Map(setMlrOption, names(defaults), defaults)
  }
  invisible(NULL)
}
