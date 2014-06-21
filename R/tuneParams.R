#FIXME: check whether optimization can be paralleized if req. by user
# FIXME check trafo
# FIXME: example missing

#' @title Hyperparameter tuning.
#'
#' @description
#' Optimizes the hyperparameters of a learner.
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
#' @template arg_measures_opt
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param control [\code{\link{TuneControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @template arg_showinfo
#' @return [\code{\link{TuneResult}}].
#' @family tune
#' @export
tuneParams = function(learner, task, resampling, measures, par.set, control, show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  checkArg(task, "SupervisedTask")
  measures = checkMeasures(measures, learner)
  checkArg(par.set, "ParamSet")
  checkArg(control, "TuneControl")
  if (!inherits(resampling, "ResampleDesc") &&  !inherits(resampling, "ResampleInstance"))
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance)
    resampling = makeResampleInstance(resampling, task = task)
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)
  checkTunerParset(learner, par.set, control)
  cl = as.character(class(control))[1]
  sel.func = switch(cl,
    TuneControlGrid = tuneGrid,
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


