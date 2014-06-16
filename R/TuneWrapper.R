#' @title Fuse learner with tuning.
#'
#' @description
#' Fuses a base learner with a search strategy to select its hyperparameters.
#' Creates a learner object, which can be used like any other learner object,
#' but which internally uses \code{\link{tuneParams}}.
#' If the train function is called on it,
#' the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the
#' complete training data with these optimal hyperparameters and returned.
#' See \code{\link{tuneParams}} for more details.
#'
#' After training, the optimal hyperparameters (and other related information) can be retrieved with
#' \code{\link{getTuneResult}}.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param resampling [\code{\link{ResampleInstance}} | \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are
#'   evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at \code{\link{TuneControl}}.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function
#'   is optimized during tuning, others are simply evaluated.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Collection of parameters and their constraints for optimization.
#' @param control [\code{\link{TuneControl}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each hyperparameter evaluation?
#'   Default is \code{TRUE}.
#' @return [\code{\link{Learner}}].
#' @export
#' @family tune
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.ksvm")
#' # stupid mini grid
#' ps = makeParamSet(
#'   makeDiscreteParam("C", values = 1:2),
#'   makeDiscreteParam("sigma", values = 1:2)
#' )
#' ctrl = makeTuneControlGrid()
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
#' mod = train(lrn, task)
#' print(getTuneResult(mod))
#' # nested resampling for evaluation
#' # we also extract tuned hyper pars in each iteration
#' r = resample(lrn, task, outer, extract = getTuneResult)
#' print(r$extract)
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info = TRUE) {
  checkArg(learner, "Learner")
  checkArg(resampling, c("ResampleDesc", "ResampleInstance"))
  if (missing(measures)) {
    measures = default.measures(learner)
  } else {
    if (is(measures, "Measure"))
      measures = list(measures)
    else
      checkListElementClass(measures, "Measure")
  }
  checkArg(par.set, "ParamSet")
  checkArg(control, "TuneControl")
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)
  id = paste(learner$id, "tuned", sep = ".")
  x = makeOptWrapper(id, learner, resampling, measures, par.set, character(0L),
    function(){}, control, show.info, "TuneWrapper")
  checkTunerParset(learner, par.set, control)
  return(x)
}

#' @export
trainLearner.TuneWrapper = function(.learner, .task, .subset,  ...) {
  .task = subsetTask(.task, .subset)
  or = tuneParams(.learner$next.learner, .task, .learner$resampling, .learner$measures,
    .learner$opt.pars, .learner$control, .learner$show.info)
  lrn = setHyperPars(.learner$next.learner, par.vals = or$x)
  m = train(lrn, .task)
  x = makeChainModel(next.model = m, cl = "TuneModel")
  x$opt.result = or
  return(x)
}

#' @export
predictLearner.TuneWrapper = function(.learner, .model, .newdata, ...) {
  lrn = setHyperPars(.learner$next.learner,
    par.vals = .model$learner.model$opt.result$x)
  predictLearner(lrn, .model$learner.model$next.model, .newdata)
}

#' Returns the optimal hyperparameters and optimization path after training or benchmarking.
#'
#' @param object [\code{\link{WrappedModel}} | \code{BenchmarkResult}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{TuneResult}} or list of \code{\link{TuneResult}}s].
#' @family tune
#' @export
getTuneResult = function(object) {
  UseMethod("getTuneResult")
}

#' @export
getTuneResult.WrappedModel = function(object) {
  object$learner.model$opt.result
}
