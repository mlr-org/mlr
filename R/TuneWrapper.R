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
#' @template arg_learner
#' @inheritParams tuneParams
#' @template ret_learner
#' @export
#' @family tune
#' @family wrapper
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.ksvm")
#' # stupid mini grid
#' ps = makeParamSet(
#'   makeDiscreteParam("C", values = 1:2),
#'   makeDiscreteParam("sigma", values = 1:2)
#' )
#' ctrl = makeTuneControlGrid(budget = 4L)
#' inner = makeResampleDesc("Holdout")
#' outer = makeResampleDesc("CV", iters = 2)
#' lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
#' mod = train(lrn, task)
#' print(getTuneResult(mod))
#' # nested resampling for evaluation
#' # we also extract tuned hyper pars in each iteration
#' r = resample(lrn, task, outer, extract = getTuneResult)
#' print(r$extract)
#' getNestedTuneResultsOptPathDf(r)
#' getNestedTuneResultsX(r)
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  assert(checkClass(resampling, "ResampleDesc"), checkClass(resampling, "ResampleInstance"))
  measures = checkMeasures(measures, learner)
  assertClass(par.set, classes = "ParamSet")
  assertClass(control, classes = "TuneControl")
  assertFlag(show.info)
  id = paste(learner$id, "tuned", sep = ".")
  x = makeOptWrapper(id, learner, resampling, measures, par.set, character(0L),
    function(){}, control, show.info, "TuneWrapper", "TuneModel")
  checkTunerParset(learner, par.set, measures, control)
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
  lrn = setHyperPars(.learner$next.learner, par.vals = .model$learner.model$opt.result$x)
  predictLearner(lrn, .model$learner.model$next.model, .newdata, ...)
}

#' @export
makeWrappedModel.TuneWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  # set threshold in learner so it is used in predict calls from here on
  if (learner$control$tune.threshold)
    learner = setPredictThreshold(learner, learner.model$opt.result$threshold)
  x = NextMethod()
  class(x) = c("TuneModel", class(x))
  return(x)
}


