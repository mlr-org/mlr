#' Get a description of all possible parameter settings for a learner.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @return [\code{\link[ParamHelpers]{ParamSet}}].
#' @export
getParamSet = function(learner) {
  checkArg(learner, "Learner")
  UseMethod("getParamSet")
}

#'@S3method getParamSet Learner
getParamSet.Learner = function(learner) {
  checkArg(learner, "Learner")
  learner$par.set
}

#' Get current parameter settings for a learner.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param for.fun [\code{character(1)}]\cr
#'   Restrict the returned settings to hyperparameters corresponding to \code{when}
#'   the are used (see \code{\link[ParamHelpers]{LearnerParam}}).
#'   Must be a subset of: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \code{c("train", "predict", "both")}.
#' @return [\code{list}]. A named list of values.
#' @export
getHyperPars = function(learner,  for.fun=c("train", "predict", "both")) {
  checkArg(learner, "Learner")
  checkArg(for.fun, subset=c("train", "predict", "both"))
  UseMethod("getHyperPars")
}

#' @S3method getHyperPars Learner
getHyperPars.Learner = function(learner, for.fun=c("train", "predict", "both")) {
  checkArg(learner, "Learner")
  pars = learner$par.set$pars
  pv = learner$par.vals
  ns = Filter(function(x) pars[[x]]$when %in% for.fun, names(pv))
  pv[ns]
}

#' Set the hyperparameters of a learner object.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'    Optional list of named (hyper)parameter settings. The arguments in
#'    \code{...} take precedence over values in this list.
#' @return [\code{\link{Learner}}] with changed hyperparameters.
#' @export
#' @seealso See \code{\link{getHyperPars}} for a function to retrieve
#'   the currently set hyperparameters. To get a list of all hyperparameters of
#'   a learner, see the \code{par.set} slot of the \code{\link{Learner}}
#'   object.
#' @examples
#' cl1 <- makeLearner("classif.ksvm", sigma=1)
#' cl2 <- setHyperPars(cl1, sigma=10, par.vals=list(C=2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals) {
  checkArg(learner, "Learner")
  args = list(...)
  if (missing(par.vals)) {
    par.vals = list()
  } else {
    checkArg(par.vals, "list")
    if(!isProperlyNamed(par.vals))
      stop("All parameter settings have to be named arguments!")
  }
  if (length(args) > 0L) {
    if(!isProperlyNamed(args))
      stop("All parameter settings have to be named arguments!")
    par.vals = insert(par.vals, args)
  }
  setHyperPars2(learner, par.vals)
}

#' Only exported for internal use.
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param par.vals [\code{list}]\cr
#'   List of named (hyper)parameter settings.
#' @export
setHyperPars2 = function(learner, par.vals) {
  UseMethod("setHyperPars2")
}

#' @S3method setHyperPars2 Learner
setHyperPars2.Learner = function(learner, par.vals) {
  ns = names(par.vals)
  pars = learner$par.set$pars
  for (i in seq_along(par.vals)) {
    n = ns[i]
    p = par.vals[[i]]
    pd = pars[[n]]
    if (is.null(pd)) {
      # no description: stop warn or quiet
      msg = sprintf("%s: Setting parameter %s without available description object!\nYou can switch off this check by using configureMlr!", learner$id, n)
      opwd = getOption("mlr.on.par.without.desc")
      if (opwd == "stop")
        stop(msg)
      if (opwd == "warn")
        warning(msg)
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id=n)
      learner$par.vals[[n]] = p
    } else {
      if (!isFeasible(pd, p))
        stopf("%s is not feasible for parameter '%s'!", 
          convertToShortString(p), pd$id)
      ## if valname of discrete par was used, transform it to real value
      #if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
      learner$par.vals[[n]] = p
    }
  }
  return(learner)
}

#' Set the type of predictions the learner should return.
#'
#' Possible prediction types are:
#' Classification: Labels or class probabilities (including labels).
#' Regression: Numeric or response or standard errors (including numeric response).
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} or \dQuote{prob}.
#'   Regression: \dQuote{response} or \dQuote{se}.
#'   Default is \dQuote{response}.
#' @return [\code{\link{Learner}}] with changed prediction behaviour.
#' @seealso \code{\link{setThreshold}} to alter the threshold used for prediction.
#' @export
setPredictType = function(learner, predict.type) {
  checkArg(learner, "Learner")
  checkArg(predict.type, choices=switch(learner$type,
    classif = c("response", "prob"),
    regr = c("response", "se")
  ))
  if (predict.type == "prob" && !learner$prob)
    stopf("Trying to predict probs, but %s does not support that!", learner$id)
  if (predict.type == "se" && !learner$se)
    stopf("Trying to predict standard errors, but %s does not support that!", learner$id)
  learner$predict.type = predict.type
  return(learner)
}


# FIXME what if hyper pars are of complx type?
getHyperParsString = function(learner) {
  hps = getHyperPars(learner)
  ns = names(hps)
  pars = getParamSet(learner)$pars[ns]
  s = Map(paramValueToString, pars, hps)
  paste(ns, s, sep = "=", collapse = ",")
}
