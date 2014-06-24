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
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info = getMlrOption("show.info")) {
  learner = checkLearner(learner)
  checkArg(resampling, c("ResampleDesc", "ResampleInstance"))
  measures = checkMeasures(measures, learner)
  assertClasses(par.set, classes = "ParamSet")
  assertClass(control, classes = "TuneControl")
  assertLogical(show.info, len = 1L, any.missing = FALSE)
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

