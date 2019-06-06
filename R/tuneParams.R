# FIXME: check whether optimization can be paralleized if req. by user

#' @title Hyperparameter tuning.
#'
#' @description
#' Optimizes the hyperparameters of a learner.
#' Allows for different optimization methods, such as grid search, evolutionary strategies,
#' iterated F-race, etc. You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at
#' [TuneControl].
#'
#' Multi-criteria tuning can be done with [tuneParamsMultiCrit].
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling ([ResampleInstance] | [ResampleDesc])\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are
#'   evaluated on the same training/test sets.
#'   If you want to change that behavior, look at [TuneControl].
#' @template arg_measures_opt
#' @param par.set ([ParamHelpers::ParamSet])\cr
#'   Collection of parameters and their constraints for optimization.
#'   Dependent parameters with a `requires` field must use `quote` and not
#'   `expression` to define it.
#' @param control ([TuneControl])\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @template arg_showinfo
#' @param resample.fun ([closure])\cr
#'   The function to use for resampling. Defaults to [resample]. If a user-given function
#'   is to be used instead, it should take the arguments \dQuote{learner}, \dQuote{task}, \dQuote{resampling},
#'   \dQuote{measures}, and \dQuote{show.info}; see [resample]. Within this function,
#'   it is easiest to call [resample] and possibly modify the result.
#'   However, it is possible to return a list with only the following essential slots:
#'   the \dQuote{aggr} slot for general tuning, additionally the \dQuote{pred} slot if threshold tuning is performed
#'   (see [TuneControl]), and the \dQuote{err.msgs} and \dQuote{err.dumps} slots for error reporting.
#'   This parameter must be the default when `mbo` tuning is performed.
#' @return ([TuneResult]).
#' @family tune
#' @note If you would like to include results from the training data set, make
#' sure to appropriately adjust the resampling strategy and the aggregation for
#' the measure. See example code below.
#' @export
#' @examples
#' set.seed(123)
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
#' # access data for all evaluated points
#' df = as.data.frame(res$opt.path)
#' df1 = as.data.frame(res$opt.path, trafo = TRUE)
#' print(head(df[, -ncol(df)]))
#' print(head(df1[, -ncol(df)]))
#' # access data for all evaluated points - alternative
#' df2 = generateHyperParsEffectData(res)
#' df3 = generateHyperParsEffectData(res, trafo = TRUE)
#' print(head(df2$data[, -ncol(df2$data)]))
#' print(head(df3$data[, -ncol(df3$data)]))
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
#' ctrl = makeTuneControlIrace(maxExperiments = 5, nbIterations = 1, minNbSurvival = 1)
#' rdesc = makeResampleDesc("Holdout")
#' res = tuneParams("classif.ksvm", iris.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' df = as.data.frame(res$opt.path)
#' print(head(df[, -ncol(df)]))
#'
#' # include the training set performance as well
#' rdesc = makeResampleDesc("Holdout", predict = "both")
#' res = tuneParams("classif.ksvm", iris.task, rdesc, par.set = ps,
#'   control = ctrl, measures = list(mmce, setAggregation(mmce, train.mean)))
#' print(res)
#' df2 = as.data.frame(res$opt.path)
#' print(head(df2[, -ncol(df2)]))
#' }
#' @seealso [generateHyperParsEffectData]
tuneParams = function(learner, task, resampling, measures, par.set, control,
  show.info = getMlrOption("show.info"), resample.fun = resample) {

  learner = checkLearner(learner)
  assertClass(task, classes = "Task")
  measures = checkMeasures(measures, learner)
  assertClass(par.set, classes = "ParamSet")
  assertClass(control, classes = "TuneControl")
  assertFunction(resample.fun)
  if (!inherits(resampling, "ResampleDesc") && !inherits(resampling, "ResampleInstance")) {
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  }
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance) {
    resampling = makeResampleInstance(resampling, task = task)
  }
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

  need.extra = control$tune.threshold || getMlrOption("on.error.dump")
  opt.path = makeOptPathDFFromMeasures(par.set, measures, include.extra = need.extra)
  if (show.info) {
    messagef("[Tune] Started tuning learner %s for parameter set:", learner$id)
    message(printToChar(par.set)) # using message() since this can go over the char limit of messagef(), see issue #1528
    messagef("With control class: %s", cl)
    messagef("Imputation value: %g", control$impute.val)
  }

  or = sel.func(learner, task, resampling, measures, par.set, control,
    opt.path, show.info, resample.fun)
  if (show.info) {
    messagef("[Tune] Result: %s : %s", paramValueToString(par.set, or$x), perfsToString(or$y))
  }
  return(or)
}


#' @title Get the optimization path of a tuning result.
#'
#' @description
#' Returns the opt.path from a ([TuneResult]) object.
#' @param tune.result ([TuneResult]) \cr
#'   A tuning result of the ([tuneParams]) function.
#' @param as.df (`logical(1)`)\cr
#'   Should the optimization path be returned as a data frame?
#'   Default is `TRUE`.
#' @return ([ParamHelpers::OptPath]) or ([data.frame]).
#' @export
getTuneResultOptPath = function(tune.result, as.df = TRUE) {
  if (as.df == TRUE) {
    return(as.data.frame(tune.result$opt.path))
  } else {
    return(tune.result$opt.path)
  }
}
