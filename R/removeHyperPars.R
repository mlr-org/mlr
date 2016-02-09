#' @title Remove hyperparameters settings of a learner.
#'
#' @description
#' Remove settings (previously set through mlr) for some parameters.
#' Which means that the default behavior for that param will now be used.
#'
#' @template arg_learner
#' @param ids [\code{character}]\cr
#'   Parameter names to remove settings for.
#'    Default is \code{character(0L)}.
#' @template ret_learner
#' @export
#' @family learner
removeHyperPars = function(learner, ids = character(0L)) {
  assertClass(learner, classes = "Learner")
  assertCharacter(ids, any.missing = FALSE)
  d = setdiff(ids, names(getHyperPars(learner)))
  if (length(d) > 0L)
    stopf("Trying to remove param settings which were not set before: %s", collapse(d))
  UseMethod("removeHyperPars")
}

#' @export
removeHyperPars.Learner = function(learner, ids = character(0L)) {
  learner$par.vals[ids] = NULL
  on.par.out.of.bounds = coalesce(learner$config$on.par.out.of.bounds, getMlrOptions()$on.par.out.of.bounds)
  if (on.par.out.of.bounds != "quiet") {}
  
  feasibility = TRUE
  if (length(learner$par.vals[ids]) > 0 && !(feasibility = isFeasible(learner$par.set, learner$par.vals, use.defaults = TRUE, filter = TRUE))) {
    stopfun = switch(on.par.out.of.bounds, stop = stop, warn = warning, function(...) {})
    msg = coalesce(attr(feasibility, "warning"), "")
    stopfun(msg)
  }
  return(learner)
}


