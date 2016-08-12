#' @title Get current parameter settings for a learner.
#'
#' @description
#' Retrieves the current hyperparameter settings of a learner.
#'
#' @template arg_learner
#' @param for.fun [\code{character(1)}]\cr
#'   Restrict the returned settings to hyperparameters corresponding to \code{when}
#'   the are used (see \code{\link[ParamHelpers]{LearnerParam}}).
#'   Must be a subset of: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \code{c("train", "predict", "both")}.
#' @return [\code{list}]. A named list of values.
#' @family learner
#' @export
getHyperPars = function(learner,  for.fun = c("train", "predict", "both")) {
  assertClass(learner, classes = "Learner")
  assertSubset(for.fun, choices = c("train", "predict", "both"))
  UseMethod("getHyperPars")
}

#' @export
getHyperPars.Learner = function(learner, for.fun = c("train", "predict", "both")) {
  assertClass(learner, classes = "Learner")
  pars = learner$par.set$pars
  pv = learner$par.vals
  ns = Filter(function(x) pars[[x]]$when %in% for.fun, names(pv))
  pv[ns]
}

getHyperParsString = function(learner, show.missing.values) {
  hps = getHyperPars(learner)
  ns = names(hps)
  pars = getParamSet(learner)$pars[ns]
  s = mapply(paramValueToString, pars, hps, MoreArgs = list(show.missing.values = show.missing.values))
  stri_paste(ns, s, sep = "=", collapse = ",")
}

# This is required for featureless learnerns. A measure is used as a parameter and we have to print
# the id instead of the name of the object
paramValueToString.MeasureParam = function(par, x, ...) {
  as.character(x$id)
}

