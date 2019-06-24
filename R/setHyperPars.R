#' Set the hyperparameters of a learner object.
#'
#' @template arg_learner
#' @param ... (any)\cr Optional named (hyper)parameters. If you want to set
#'   specific hyperparameters for a learner during model creation, these should
#'   go here. You can get a list of available hyperparameters using
#'   `getParamSet(<learner>)`. Alternatively hyperparameters can be given using
#'   the `par.vals` argument but `...` should be preferred!
#' @param par.vals ([list])\cr Optional list of named (hyper)parameters. The
#'   arguments in `...` take precedence over values in this list. We strongly
#'   encourage you to use `...` for passing hyperparameters.
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
setHyperPars = function(learner, ..., par.vals = list()) {
  args = list(...)
  assertList(args, names = "unique", .var.name = "parameter settings")
  assertList(par.vals, names = "unique", .var.name = "parameter settings")
  setHyperPars2(learner, insert(par.vals, args))
}

#' Only exported for internal use.
#' @param learner ([Learner])\cr
#'   The learner.
#' @param par.vals ([list])\cr
#'   List of named (hyper)parameter settings.
#' @export
setHyperPars2 = function(learner, par.vals) {
  UseMethod("setHyperPars2")
}

#' @export
setHyperPars2.Learner = function(learner, par.vals) {

  if (length(par.vals) == 0L) {
    return(learner)
  }

  ns = names(par.vals)
  pars = learner$par.set$pars
  on.par.without.desc = coalesce(learner$config$on.par.without.desc, getMlrOption("on.par.without.desc"))
  on.par.out.of.bounds = coalesce(learner$config$on.par.out.of.bounds, getMlrOption("on.par.out.of.bounds"))

  for (i in seq_along(par.vals)) {
    n = ns[i]
    p = par.vals[[i]]
    pd = pars[[n]]
    if (is.null(pd)) {
      if (on.par.without.desc != "quiet") {
        # no description: stop warn or quiet
        msg = sprintf("%s: Setting parameter %s without available description object!", learner$id, n)
        # since we couldn't find the par let's look for 3 most similar
        parnames = names(pars)
        indices = head(order(adist(n, parnames)), 3L)
        possibles = parnames[indices]
        if (length(possibles) > 0L) {
          msg = paste0(msg, sprintf("\nDid you mean one of these hyperparameters instead: %s", stri_flatten(possibles, collapse = " ")))
        }
        msg = paste0(msg, "\nYou can switch off this check by using configureMlr!")
      }

      if (on.par.without.desc == "stop") {
        stop(msg)
      } else if (on.par.without.desc == "warn") {
        warning(msg)
      }
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id = n)
      learner$par.vals[[n]] = p
    } else {
      if (on.par.out.of.bounds != "quiet" && !isFeasible(pd, p)) {
        msg = sprintf("%s is not feasible for parameter '%s'!", convertToShortString(p), pd$id)
        if (on.par.out.of.bounds == "stop") {
          stop(msg)
        } else {
          warning(msg)
        }
      }

      ## if valname of discrete par was used, transform it to real value
      # if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
      learner$par.vals[[n]] = p
    }
  }

  return(learner)
}
