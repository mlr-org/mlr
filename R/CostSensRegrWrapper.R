#' @title Wraps a regression learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via \code{\link{getCostSensRegrModels}}.
#'
#' For each class in the task, an individual regression model is fitted for the costs of that class.
#' During prediction, the class with the lowest predicted costs is selected.
#'
#' @template arg_learner_regr
#' @template ret_learner
#' @export
#' @family costsens
#' @family wrapper
#' @aliases CostSensRegrWrapper CostSensRegrModel
makeCostSensRegrWrapper = function(learner) {
  learner = checkLearnerRegr(learner)
  # we cannot make use of 'se' here
  learner = setPredictType(learner, "response")
  id = paste("costsens", learner$id, sep = ".")
  x = makeBaseWrapper(id, learner, package = learner$package, cl = "CostSensRegrWrapper")
  x$type = "costsens"
  removeProperties(x, c("weights", "se", "prob"))
}

#' @export
trainLearner.CostSensRegrWrapper = function(.learner, .task, .subset, ...) {
  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  costs = .task$env$costs
  classes = .task$task.desc$class.levels
  feats = .task$env$data
  models = vector("list", length = length(classes))
  for (i in seq_along(classes)) {
    cl = classes[i]
    y = costs[, cl]
    data = cbind(feats, ..y.. = y)
    task = makeRegrTask(id = cl, data = data, target = "..y..",
      check.data = FALSE, fixup.data = "quiet")
    models[[i]] = train(.learner$next.learner, task)
  }
  makeChainModel(next.model = models, cl = "CostSensRegrModel")
}

#' @export
predictLearner.CostSensRegrWrapper = function(.learner, .model, .newdata, ...) {
  classes = .model$task.desc$class.levels
  models = getCostSensRegrModels(.model)
  preds = sapply(models, function(mod) {
    predict(mod, newdata = .newdata, ...)$data$response
  })
  # FIXME: this will break for length(models) == 1? do not use sapply!
  preds = apply(preds, 1L, getMinIndex)
  return(factor(classes[preds], levels = classes))
}


#' @export
makeWrappedModel.CostSensRegrWrapper = function(learner, learner.model, task.desc, subset, features,
  factor.levels, time) {

  x = NextMethod()
  addClasses(x, "CostSensRegrModel")
}


#' Returns the list of fitted models.
#'
#' @param model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training a cost-sensitive regression learner.
#' @param learner.models [\code{logical(1)}]\cr
#'   Return underlying R models or wrapped
#'   mlr models (\code{\link[mlr]{WrappedModel}}).
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
getCostSensRegrModels = function(model, learner.models = FALSE) {
  assertClass(model, classes = "CostSensRegrModel")
  ms = model$learner.model$next.model
  if (learner.models)
    extractSubList(ms, "learner.model", simplify = FALSE)
  else
    ms
}
