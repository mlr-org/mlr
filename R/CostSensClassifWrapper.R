#' @title Wraps a classification learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' The classification model can easily be accessed via \code{\link{getCostSensClassifModel}}.
#'
#' This is a very naive learner, where the costs are transformed into classification labels -
#' the label for each case is the name of class with minimal costs.
#' Then the classifier is fitted to that data and subsequently used for prediction.
#'
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   The basic classification learner.
#' @return [\code{\link[mlr]{Learner}}].
#' @export
makeCostSensClassifWrapper = function(learner) {
  checkLearnerClassif(learner)
  learner = setPredictType(learner, "response")
  id = paste("costsens", learner$id, sep = ".")
  x = makeBaseWrapper(id, learner, package = learner$packages, cl = "CostSensClassifWrapper")
  x$type = "costsens"
  x$weights = FALSE
  x$se = FALSE
  x$prob = FALSE
  x
}

#' @S3method trainLearner CostSensClassifWrapper
trainLearner.CostSensClassifWrapper = function(.learner, .task, .subset, ...) {
  .task = subsetTask(.task, subset = .subset)
  costs = .task$env$costs
  classes = .task$task.desc$class.levels
  j = getMinIndexOfRows(costs)
  newy = classes[j]
  feats = .task$env$data
  #FIXME name clash
  data = cbind(feats, .y = newy)
  task = makeClassifTask(data = data, target = ".y")
  model = train(.learner$next.learner, task)
  makeChainModel(next.model = model, cl = "CostSensClassifModel")
}

#' @S3method makeWrappedModel CostSensClassifWrapper
makeWrappedModel.CostSensClassifWrapper = function(learner, model, task.desc, subset, vars, features, time) {
  x = NextMethod()
  class(x) = c("CostSensClassifModel", class(x))
  return(x)
}


#' Returns the underlying classification model.
#'
#' @param model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training a cost-sensitive classification learner.
#' @param learner.models [\code{logical(1)}]\cr
#'   Return underlying R model or wrapped
#'   mlr model (\code{\link[mlr]{WrappedModel}}).
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
getCostSensClassifModel= function(model, learner.models = TRUE) {
  checkArg(model, "CostSensClassifModel")
  m = model$learner.model$next.model
  if (learner.model)
    m$learner.model
  else
    m
}


