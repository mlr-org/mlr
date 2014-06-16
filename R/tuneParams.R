#FIXME: check whether optimization can be paralleized if req. by user
# FIXME check trafo
# FIXME: example missing

#' Hyperparameter tuning.
#'
#' Optimizes the hyperparameters of a learner for a classification or regression problem.
#' Allows for different optimization methods, such as grid search, evolutionary strategies, etc.
#' You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at
#' \code{\link{TuneControl}}.
#'
#' Note that if tranformations are associated with the parameters, the returned result will contain
#' transformed values in the optimal result and the path.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling [\code{\link{ResampleInstance}} | \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are
#'   evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at \code{\link{TuneControl}}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param control [\code{\link{TuneControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function
#'   is optimized during tuning, others are simply evaluated.
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each hyperparameter evaluation?
#'   Default is \code{TRUE}.
#' @return [\code{\link{TuneResult}}].
#' @family tune
#' @export
tuneParams = function(learner, task, resampling, measures, par.set, control, show.info=TRUE) {
  learner = checkLearner(learner)
  checkArg(task, "SupervisedTask")
  if (missing(measures))
    measures = default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)
  checkListElementClass(measures, "Measure")
  checkArg(par.set, "ParamSet")
  checkArg(control, "TuneControl")
  if (!inherits(resampling, "ResampleDesc") &&  !inherits(resampling, "ResampleInstance"))
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  checkTunerParset(learner, par.set, control)
  cl = as.character(class(control))[1]
	sel.func = switch(cl,
    TuneControlGrid = tuneGrid,
    TuneControlOptim = tuneOptim,
    TuneControlCMAES = tuneCMAES,
    TuneControlMBO = tuneMBO,
	  TuneControlIrace = tuneIrace,
	  TuneControlRandom = tuneRandom
	)
  opt.path = makeOptPathDFFromMeasures(par.set, measures)
  if (show.info) {
    messagef("[Tune] Started tuning learner %s for parameter set:", learner$id)
    messagef(printToChar(par.set))
    messagef("With control class: %s",  cl)
  }
  or = sel.func(learner, task, resampling, measures, par.set, control, opt.path, show.info)
  if (show.info)
    messagef("[Tune] Result: %s : %s", paramValueToString(par.set, or$x), perfsToString(or$y))
	return(or)
}


