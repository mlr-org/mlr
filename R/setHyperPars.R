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
#' cl1 = makeLearner("classif.ksvm", sigma = 1)
#' cl2 = setHyperPars(cl1, sigma = 10, par.vals = list(C = 2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals) {
  assertClass(learner, classes = "Learner")
  args = list(...)
  if (missing(par.vals)) {
    par.vals = list()
  } else {
    assertList(par.vals)
    if(!isProperlyNamed(par.vals))
      stop("All parameter settings have to be named arguments!")
  }
  if (length(args)) {
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

#' @export
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
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id = n)
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

