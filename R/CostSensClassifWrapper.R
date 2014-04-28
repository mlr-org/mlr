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
  # case of equals best costs, do random sampling
  newy = getMinIndexOfRows(costs)
  newy = classes[newy]
  feats = .task$env$data
  #FIXME: name clash
  data = cbind(feats, .y = newy)
  # remove stuff with completely equal costs from training
  remove = apply(costs, 1, function(x) length(unique(x)) == 1)
  data = data[-remove, ]
  task = makeClassifTask(data = data, target = ".y")
  model = train(.learner$next.learner, task)
  makeChainModel(next.model = model, cl = "CostSensClassifModel")
}

#' @S3method makeWrappedModel CostSensClassifWrapper
makeWrappedModel.CostSensClassifWrapper = function(learner, model, task.desc, subset, features,
  factor.levels, time) {

  x = NextMethod()
  class(x) = c("CostSensClassifModel", class(x))
  return(x)
}


#' Returns the underlying classification model.
#'
#' @param model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training a cost-sensitive classification learner.
#' @param learner.model [\code{logical(1)}]\cr
#'   Return underlying R model or wrapped
#'   mlr model (\code{\link[mlr]{WrappedModel}}).
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
getCostSensClassifModel= function(model, learner.model = TRUE) {
  checkArg(model, "CostSensClassifModel")
  checkArg(learner.model, "logical", len = 1L, na.ok = FALSE)
  m = model$learner.model$next.model
  if (learner.model)
    m$learner.model
  else
    m
}


