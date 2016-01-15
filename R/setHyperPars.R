#' Set the hyperparameters of a learner object.
#'
#' @template arg_learner
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameter settings. The arguments in
#'   \code{...} take precedence over values in this list.
#' @param reset [\code{character(1)}]\cr
#'   The options are \code{"no"} to add and in case overwrite param settings, which is the default.
#'   \code{"soft"} will reset the manualy set param.values and restore the mlr defaults.
#'   \code{"hard"} will reset all param.values.
#' @template ret_learner
#' @export
#' @family learner
#' @examples
#' cl1 = makeLearner("classif.ksvm", sigma = 1)
#' cl2 = setHyperPars(cl1, sigma = 10, par.vals = list(C = 2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals = list(), reset = "no") {
  args = list(...)
  assertClass(learner, classes = "Learner")
  assertList(args, names = "named", .var.name = "parameter settings")
  assertList(par.vals, names = "named", .var.name = "parameter settings")
  assertChoice(reset, c("no", "soft", "hard"))
  setHyperPars2(learner, insert(par.vals, args))
}

#' Only exported for internal use.
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param par.vals [\code{list}]\cr
#'   List of named (hyper)parameter settings.
#' @export
setHyperPars2 = function(learner, par.vals, reset = "no") {
  UseMethod("setHyperPars2")
}

#' @export
setHyperPars2.Learner = function(learner, par.vals, reset = "no") {
  #load mlr-default pars of learner
  if (reset == "soft") {
    par.vals = insert(learner$mlr.default.par.vals, par.vals)  
  } else if (reset == "no") {
    par.vals = insert(insert(learner$mlr.default.par.vals, learner$par.vals), par.vals)
  } else if (reset == "hard") {
    par.vals = par.vals
  } else if (reset == "before") {
    par.vals = insert(learner$par.vals, par.vals)
  }
  
  ns = names(par.vals)
  pars = learner$par.set$pars
  on.par.without.desc = coalesce(learner$config$on.par.without.desc, getMlrOptions()$on.par.without.desc)
  on.par.out.of.bounds = coalesce(learner$config$on.par.out.of.bounds, getMlrOptions()$on.par.out.of.bounds)
  for (i in seq_along(par.vals)) {
    n = ns[i]
    pd = pars[[n]]
    if (is.null(pd)) {
      # no description: stop warn or quiet
      msg = sprintf("%s: Setting parameter %s without available description object!\nYou can switch off this check by using configureMlr!", learner$id, n)
      if (on.par.without.desc == "stop") {
        stop(msg)
      } else if (on.par.without.desc == "warn") {
        warning(msg)
      }
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id = n)
    } else {
      if (on.par.out.of.bounds != "quiet" && !isFeasible(pd, par.vals[[i]])) {
        msg = sprintf("%s is not feasible for parameter '%s'!", convertToShortString(par.vals[[i]]), pd$id)
        if (on.par.out.of.bounds == "stop") {
          stop(msg)
        } else {
          warning(msg)
        }
      }
      ## if valname of discrete par was used, transform it to real value
      #if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
    }
  }
  if (length(par.vals) > 0 && !isFeasible(learner$par.set, par.vals, use.defaults = TRUE, filter = TRUE, warn = TRUE)) {
    if (on.par.out.of.bounds == "stop") {
      stop()
    }
  }
  learner$par.vals = par.vals
  return(learner)
}
