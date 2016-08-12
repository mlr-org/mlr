#' Set the hyperparameters of a learner object.
#'
#' @template arg_learner
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameter settings. The arguments in
#'   \code{...} take precedence over values in this list.
#' @template ret_learner
#' @note If a named (hyper)parameter can't be found for the given learner, the 3
#' closest (hyper)parameter names will be output in case the user mistyped.
#' @export
#' @details Note that learners can also contain task dependent expressions, which can be based on any
#' information provided by the task. For convenience, the most often used keys are available directly
#' \itemize{
#'   \item{\code{task}:} the task itself, allowing to access any of its elements
#'   \item{\code{p}:} the number of features in the task
#'   \item{\code{n}:} the number of observations in the task
#'   \item{\code{type}:} the task type, i.e. "classif", "regr", "surv", "cluster", "costcens" or "multilabel"
#'   \item{\code{k}:} the number of classes of the target variable (only available for classification tasks)
#' }
#' However, if one wants to access any other parts of the \code{task}, one can do so. For instance, one could
#' access the "blocking" via \code{task$task.desc$has.blocking}.
#' @family learner
#' @importFrom utils adist
#' @examples
#' cl1 = makeLearner("classif.ksvm", sigma = 1)
#' cl2 = setHyperPars(cl1, sigma = 10, par.vals = list(C = 2))
#' cl3 = setHyperPars(cl2, C = expression(round(n / p)))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
#' print(cl3)
setHyperPars = function(learner, ..., par.vals = list()) {
  args = list(...)
  assertClass(learner, classes = "Learner")
  assertList(args, names = "named", .var.name = "parameter settings")
  assertList(par.vals, names = "named", .var.name = "parameter settings")
  setHyperPars2(learner, insert(par.vals, args))
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
  # ensure that even the empty list is named, we had problems here, see #759
  if (is.null(ns) && is.null(names(learner$par.vals))) {
    names(learner$par.vals) = character(0)
  }
  pars = learner$par.set$pars
  on.par.without.desc = coalesce(learner$config$on.par.without.desc, getMlrOptions()$on.par.without.desc)
  on.par.out.of.bounds = coalesce(learner$config$on.par.out.of.bounds, getMlrOptions()$on.par.out.of.bounds)
  for (i in seq_along(par.vals)) {
    n = ns[i]
    p = par.vals[[i]]
    pd = pars[[n]]
    if (is.null(pd)) {
      if (on.par.without.desc != "quiet") {
        # no description: stop warn or quiet
        msg = sprintf("%s: Setting parameter %s without available description object!",
          learner$id, n)
        # since we couldn't find the par let's look for 3 most similar
        parnames = names(pars)
        indices = head(order(adist(n, parnames)), 3L)
        possibles = parnames[indices]
        if (length(possibles) > 0L) {
          msg = paste(msg, sprintf("\nDid you mean one of these hyperparameters instead: %s", stri_flatten(possibles, collapse = " ")))
        }
        msg = paste(msg, "\nYou can switch off this check by using configureMlr!")
      }

      if (on.par.without.desc == "stop") {
        stop(msg)
      } else if (on.par.without.desc == "warn") {
        warning(msg)
      }
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id = n)
      learner$par.vals[[n]] = p
    } else {
      if (on.par.out.of.bounds != "quiet" && !isFeasible(pd, p) && !is.expression(p)) {
        msg = sprintf("%s is not feasible for parameter '%s'!", convertToShortString(p), pd$id)
        if (on.par.out.of.bounds == "stop") {
          stop(msg)
        } else {
          warning(msg)
        }
      }
      ## if valname of discrete par was used, transform it to real value
      #if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
      learner$par.vals[[n]] = p
    }
  }
  return(learner)
}
