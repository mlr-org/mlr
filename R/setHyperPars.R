#' Set the hyperparameters of a learner object.
#'
#' @template arg_learner
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameter settings. The arguments in
#'   \code{...} take precedence over values in this list.
#' @param on.par.without.desc [\code{character(1)}]\cr
#'   Locally overrule this option. See \code{\link{configureMlr}} for details.
#' @param on.par.out.of.bounds [\code{character(1)}]\cr
#'   Locally overrule this option. See \code{\link{configureMlr}} for details.
#' @template ret_learner
#' @export
#' @family learner
#' @examples
#' cl1 = makeLearner("classif.ksvm", sigma = 1)
#' cl2 = setHyperPars(cl1, sigma = 10, par.vals = list(C = 2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals = list(),
  on.par.without.desc = getMlrOption("on.par.without.desc"),
  on.par.out.of.bounds = getMlrOption("on.par.out.of.bounds")) {
  args = list(...)
  assertClass(learner, classes = "Learner")
  assertList(args, names = "named", .var.name = "parameter settings")
  assertList(par.vals, names = "named", .var.name = "parameter settings")
  setHyperPars2(learner, insert(par.vals, args), on.par.without.desc, on.par.out.of.bounds)
}

#' Only exported for internal use.
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param par.vals [\code{list}]\cr
#'   List of named (hyper)parameter settings.
#' @param on.par.without.desc [\code{character(1)}]\cr
#'   Locally overrule this option. See \code{\link{configureMlr}} for details.
#' @param on.par.out.of.bounds [\code{character(1)}]\cr
#'   Locally overrule this option. See \code{\link{configureMlr}} for details.
#' @export
setHyperPars2 = function(learner, par.vals,
  on.par.without.desc = getMlrOption("on.par.without.desc"),
  on.par.out.of.bounds = getMlrOption("on.par.out.of.bounds")) {
  UseMethod("setHyperPars2")
}

#' @export
setHyperPars2.Learner = function(learner, par.vals,
  on.par.without.desc = getMlrOption("on.par.without.desc"),
  on.par.out.of.bounds = getMlrOption("on.par.out.of.bounds")) {
  ns = names(par.vals)
  pars = learner$par.set$pars
  for (i in seq_along(par.vals)) {
    n = ns[i]
    p = par.vals[[i]]
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
      #if (pd$type == "discrete" && is.character(p) && length(p) == 1 && p %in% names(pd$values))
      #  p = pd$values[[p]]
      learner$par.vals[[n]] = p
    }
  }
  return(learner)
}
