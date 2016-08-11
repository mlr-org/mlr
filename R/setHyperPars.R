#' @title Set the hyperparameters of a learner object.
#'
#' @description
#' Takes the possiby defined mlr defsault of a learners and inserts/overwrites
#' with the new settings.
#'
#' @inheritParams setHyperPars2
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameter settings. The arguments in
#'   \code{...} take precedence over values in this list.
#' @param use.mlr.defaults [\code{list}]\cr
#'   Use the mlr.defaults for the given learner.
#'   This is recommended as they guarantee that the learner works and the underlying function is called with all necessary arguments.
#'   Default is \code{TRUE} which means that the mlr.defaults will be kept as long as they are feasible.
#' @param update
#'   Whether to update the existing parameter values of the learner or to just give respect to the new ones.
#'   Default is \code{TRUE} which means that the old parameter values will be kept as long as they are feasible.
#' @template ret_learner
#' @note If a named (hyper)parameter can't be found for the given learner, the 3
#' closest (hyper)parameter names will be output in case the user mistyped.
#' @export
#' @family learner
#' @importFrom utils adist
#' @examples
#' cl1 = makeLearner("classif.ksvm", sigma = 1)
#' cl2 = setHyperPars(cl1, sigma = 10, par.vals = list(C = 2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals = list(), use.mlr.defaults = TRUE, update = TRUE) {
  args = list(...)
  assertClass(learner, classes = "Learner")
  assertList(args, names = "named", .var.name = "parameter settings")
  assertList(par.vals, names = "named", .var.name = "parameter settings")
  assertFlag(use.mlr.defaults)
  assertFlag(update)
  setHyperPars2(learner, insert(par.vals, args), use.mlr.defaults = use.mlr.defaults, update = update)
}

#' Only exported for internal use.
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @export
setHyperPars2 = function(learner, par.vals, use.mlr.defaults = TRUE, update = TRUE) {
  UseMethod("setHyperPars2")
}

#' @export
setHyperPars2.Learner = function(learner, par.vals, use.mlr.defaults = TRUE, update = TRUE) {

  #use existing par.vals
  if (update) {
    par.vals = updateParVals(getParamSet(learner), learner$par.vals, par.vals)
  }
  #load mlr-default pars of learner
  if (use.mlr.defaults) {
    mlr.defaults = coalesce(learner$mlr.defaults, list()) # Wrappers can have NULL here
    par.vals = updateParVals(getParamSet(learner), mlr.defaults, par.vals)
  }

  ns = names(par.vals)
  pars = learner$par.set$pars
  on.par.without.desc = coalesce(learner$config$on.par.without.desc, getMlrOptions()$on.par.without.desc)
  on.par.out.of.bounds = coalesce(learner$config$on.par.out.of.bounds, getMlrOptions()$on.par.out.of.bounds)
  stopfun = switch(on.par.out.of.bounds, stop = stop, warn = warning, function(...) {})
  for (i in seq_along(par.vals)) {
    n = ns[i]
    pd = pars[[n]]
    if (is.null(pd)) {
      # since we couldn't find the par let's look for 3 most similar
      parnames = names(pars)
      indices = order(adist(n, parnames))[1:3]
      possibles = na.omit(parnames[indices])
      if (length(possibles) > 0) {
        messagef("%s: couldn't find hyperparameter '%s'\nDid you mean one of these hyperparameters instead: %s",
          learner$id, n, stri_flatten(possibles, collapse = " "))
      }

      # no description: stop warn or quiet
      msg = sprintf("%s: Setting parameter %s without available description object!\nYou can switch off this check by using configureMlr!",
        learner$id, n)

      if (on.par.without.desc == "stop") {
        stop(msg)
      } else if (on.par.without.desc == "warn") {
        warning(msg)
      }
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id = n)
    } else {
      feasibility = TRUE
      if (on.par.out.of.bounds != "quiet" && !(feasibility = isFeasible(pd, par.vals[[i]]))) {
        msg = coalesce(attr(feasibility, "warning"), sprintf("%s is not feasible for parameter '%s'!", convertToShortString(par.vals[[i]]), pd$id))
        stopfun(msg)
      }
      ## if valname of discrete par was used, transform it to real value
      #if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
    }
  }
  feasibility = TRUE
  if (length(par.vals) > 0 && !(feasibility = isFeasible(learner$par.set, par.vals, use.defaults = TRUE, filter = TRUE))) {
    msg = coalesce(attr(feasibility, "warning"), "")
    stopfun(msg)
  }
  # ensure that even the empty list is named, we had problems here, see #759
  if (is.null(names(par.vals))) {
    names(par.vals) = character(0)
  }
  learner$par.vals = par.vals
  return(learner)
}
