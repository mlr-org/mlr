#FIXME: check whether optimization can be paralleized if req. by user

#' @title Hyperparameter tuning.
#'
#' @description
#' Optimizes the hyperparameters of a learner.
#' Allows for different optimization methods, such as grid search, evolutionary strategies,
#' iterated F-race, etc. You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at
#' \code{\link{TuneControl}}.
#'
#' Multi-criteria tuning can be done with \code{\link{tuneParamsMultiCrit}}.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling [\code{\link{ResampleInstance}} | \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are
#'   evaluated on the same training/test sets.
#'   If you want to change that behavior, look at \code{\link{TuneControl}}.
#' @template arg_measures_opt
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @param control [\code{\link{TuneControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @template arg_showinfo
#' @return [\code{\link{TuneResult}}].
#' @family tune
#' @export
#' @examples
#' # a grid search for an SVM (with a tiny number of points...)
#' # note how easily we can optimize on a log-scale
#' ps = makeParamSet(
#'   makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
#'   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
#' )
#' ctrl = makeTuneControlGrid(resolution = 2L)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' res = tuneParams("classif.ksvm", iris.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' print(as.data.frame(res$opt.path))
#' print(as.data.frame(trafoOptPath(res$opt.path)))
#'
#' \dontrun{
#' # we optimize the SVM over 3 kernels simultanously
#' # note how we use dependent params (requires = ...) and iterated F-racing here
#' ps = makeParamSet(
#'   makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
#'   makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
#'   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x,
#'     requires = quote(kernel == "rbfdot")),
#'   makeIntegerParam("degree", lower = 2L, upper = 5L,
#'     requires = quote(kernel == "polydot"))
#' )
#' print(ps)
#' ctrl = makeTuneControlIrace(maxExperiments = 200L)
#' rdesc = makeResampleDesc("Holdout")
#' res = tuneParams("classif.ksvm", iris.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' print(head(as.data.frame(res$opt.path)))
#' }
tuneParams = function(learner, task, resampling, measures, par.set, control, show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  assertClass(task, classes = "Task")
  measures = checkMeasures(measures, learner)
  assertClass(par.set, classes = "ParamSet")
  # FIXME: must be removed later, see PH issue #52
  checkParamSet(par.set)
  assertClass(control, classes = "TuneControl")
  if (!inherits(resampling, "ResampleDesc") &&  !inherits(resampling, "ResampleInstance"))
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance)
    resampling = makeResampleInstance(resampling, task = task)
  assertFlag(show.info)
  checkTunerParset(learner, par.set, measures, control)
  control = setDefaultImputeVal(control, measures)

  cl = getClass1(control)
  sel.func = switch(cl,
    TuneControlRandom = tuneRandom,
    TuneControlGrid = tuneGrid,
    TuneControlDesign = tuneDesign,
    TuneControlCMAES = tuneCMAES,
    TuneControlGenSA = tuneGenSA,
    TuneControlMBO = tuneMBO,
    TuneControlIrace = tuneIrace,
    stopf("Tuning algorithm for '%s' does not exist!", cl)
  )
  opt.path = makeOptPathDFFromMeasures(par.set, measures, include.extra = (control$tune.threshold))
  if (show.info) {
    messagef("[Tune] Started tuning learner %s for parameter set:", learner$id)
    messagef(printToChar(par.set))
    messagef("With control class: %s", cl)
    messagef("Imputation value: %g", control$impute.val)
  }
  or = sel.func(learner, task, resampling, measures, par.set, control, opt.path, show.info)
  if (show.info)
    messagef("[Tune] Result: %s : %s", paramValueToString(par.set, or$x), perfsToString(or$y))
  return(or)
}


