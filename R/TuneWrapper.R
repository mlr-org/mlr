#FIXME: document args

#' Fuse learner with tuning.
#'
#' Fuses a base learner with a search strategy to select its hyperparameters.
#' Creates a learner object, which can be used like any other learner object,
#' but which internally uses tune. If the train function is called on it,
#' the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the
#' complete training data with these optimal hyperparameters and returned.
#' See \code{\link{tuneParams}} for more details.
#'
#' After training, the optimal hyperparameters (and other related information) can be retrieved with
#' \code{\link{getTuneResult}}.
#'
#' @param learner [\code{\link{Learner}} or string]\cr
#'   Learning algorithm.
#' @param resampling [\code{\link{ResampleDesc}} | \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\link{Measure}}]\cr
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
#' @examples
#' task = makeClassifTask(data=iris, target="Species")
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
#' r = resample(lrn, task, outer, extract = function(model) {
#'   getTuneResult(model)$x
#' })
#' print(r$extract)
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info=TRUE) {
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
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  id = paste(learner$id, "tuned", sep=".")
  x = makeOptWrapper(id, learner, resampling, measures, par.set, character(0L),
    function(){}, control, show.info, "TuneWrapper")
  checkTunerParset(learner, par.set, control)
  return(x)
}

#' @S3method trainLearner TuneWrapper
trainLearner.TuneWrapper = function(.learner, .task, .subset,  ...) {
  .task = subsetTask(.task, .subset)
  or = tuneParams(.learner$next.learner, .task, .learner$resampling, .learner$measures,
    .learner$opt.pars, .learner$control, .learner$show.info)
  lrn = setHyperPars(.learner$next.learner, par.vals=or$x)
  m = train(lrn, .task)
  x = makeChainModel(next.model=m, cl = "TuneModel")
  x$opt.result = or
  return(x)
}

#' @S3method predictLearner TuneWrapper
predictLearner.TuneWrapper = function(.learner, .model, .newdata, ...) {
  lrn = setHyperPars(.learner$next.learner,
    par.vals=.model$learner.model$opt.result$x)
  predictLearner(lrn, .model$learner.model$next.model, .newdata)
}

#' Returns the optimal hyperparameters and optimization path.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}}.
#' @return [\code{\link{TuneResult}}].
#' @export
getTuneResult = function(model) {
  model$learner.model$opt.result
}
