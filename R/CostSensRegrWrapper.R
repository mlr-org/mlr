#' @title Wraps a regression learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via \code{\link{getCostSensRegrModels}}.
#'
#' For each class in the task, an individual regression model is fitted for the costs of that class.
#' During prediction, the class with the lowest predicted costs is selected.
#'
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   The basic regression learner.
#' @return [\code{\link[mlr]{Learner}}].
#' @export
makeCostSensRegrWrapper = function(learner) {
  checkLearnerRegr(learner)
  # we cannot make use of 'se' here
  learner = setPredictType(learner, "response")
  id = paste("costsens", learner$id, sep = ".")
  x = makeBaseWrapper(id, learner, package = learner$packages, cl = "CostSensRegrWrapper")
  x$type = "costsens"
  x$weights = FALSE
  x$se = FALSE
  x$prob = FALSE
  x
}

#' @S3method trainLearner CostSensRegrWrapper
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
    task = makeRegrTask(id = cl, data = data, target = "..y..")
    models[[i]] = train(.learner$next.learner, task)
  }
  makeChainModel(next.model = models, cl = "CostSensRegrModel")
}


#' @S3method predictLearner CostSensRegrWrapper
predictLearner.CostSensRegrWrapper = function(.learner, .model, .newdata, ...) {
  classes = .model$task.desc$class.levels
  models = getCostSensRegrModels(.model)
  preds = sapply(models, function(mod) {
    predict(mod, newdata = .newdata, ...)$data$response
  })
  preds = apply(preds, 1L, getMinIndex)
  return(factor(classes[preds], levels = classes))
}


#' @S3method makeWrappedModel CostSensRegrWrapper
makeWrappedModel.CostSensRegrWrapper = function(learner, model, task.desc, subset, features,
  factor.levels, time) {

  x = NextMethod()
  class(x) = c("CostSensRegrModel", class(x))
  return(x)
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
  checkArg(model, "CostSensRegrModel")
  ms = model$learner.model$next.model
  if (learner.models)
    extractSubList(ms, "learner.model", simplify = FALSE)
  else
    ms
}


