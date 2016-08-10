#' @title Hyperparameter tuning for multiple measures at once.
#'
#' @description
#' Optimizes the hyperparameters of a learner in a multi-criteria fashion.
#' Allows for different optimization methods, such as grid search, evolutionary strategies, etc.
#' You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at
#' \code{\link{TuneMultiCritControl}}.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling [\code{\link{ResampleInstance}} | \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are
#'   evaluated on the same training/test sets.
#'   If you want to change that behavior, look at \code{\link{TuneMultiCritControl}}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @param measures [list of \code{\link{Measure}}]\cr
#'   Performance measures to optimize simultaneously.
#' @param control [\code{\link{TuneMultiCritControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @template arg_showinfo
#' @return [\code{\link{TuneMultiCritResult}}].
#' @family tune_multicrit
#' @export
#' @examples
#' \donttest{
#' # multi-criteria optimization of (tpr, fpr) with NGSA-II
#' lrn =  makeLearner("classif.ksvm")
#' rdesc = makeResampleDesc("Holdout")
#' ps = makeParamSet(
#'   makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
#'   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
#' )
#' ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
#' res = tuneParamsMultiCrit(lrn, sonar.task, rdesc, par.set = ps,
#'   measures = list(tpr, fpr), control = ctrl)
#' plotTuneMultiCritResult(res, path = TRUE)
#' }
tuneParamsMultiCrit = function(learner, task, resampling, measures, par.set, control, show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  assertClass(task, classes = "Task")
  assertList(measures, types = "Measure", min.len = 2L)
  assertClass(par.set, classes = "ParamSet")
  assertClass(control, classes = "TuneMultiCritControl")
  if (!inherits(resampling, "ResampleDesc") &&  !inherits(resampling, "ResampleInstance"))
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance)
    resampling = makeResampleInstance(resampling, task = task)
  assertFlag(show.info)
  control = setDefaultImputeVal(control, measures)
  checkTunerParset(learner, par.set, measures, control)
  requirePackages("emoa", why = "tuneParamsMultiCrit", default.method = "load")

  cl = getClass1(control)
  sel.func = switch(cl,
    TuneMultiCritControlRandom = tuneMultiCritRandom,
    TuneMultiCritControlGrid = tuneMultiCritGrid,
    TuneMultiCritControlNSGA2 = tuneMultiCritNSGA2,
    stopf("Tuning algorithm for '%s' does not exist!", cl)
  )
  opt.path = makeOptPathDFFromMeasures(par.set, measures)
  if (show.info) {
    messagef("[Tune] Started tuning learner %s for parameter set:", learner$id)
    messagef(printToChar(par.set))
    messagef("With control class: %s", cl)
    messagef("Imputation value: %g", control$impute.val)
  }
  or = sel.func(learner, task, resampling, measures, par.set, control, opt.path, show.info)
  if (show.info)
    messagef("[Tune] Result: Points on front : %i", length(or$x))
  return(or)
}



