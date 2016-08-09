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
  pars = getParamSet(learner)$pars
  # mlr.defaults = coalesce(learner$mlr.defaults, list()) # in theory this should always be an empty list and this should not be neccessary but getHyperPars.Learner is called on some Wrappers directly #FIXME?
  # pv = updateParVals(getParamSet(learner), old.par.vals = mlr.defaults, new.par.vals = learner$par.vals)  
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

