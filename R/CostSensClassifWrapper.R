#' @title Wraps a classification learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' The classification model can easily be accessed via \code{\link{getCostSensClassifModel}}.
#'
#' This is a very naive learner, where the costs are transformed into classification labels -
#' the label for each case is the name of class with minimal costs.
#' (If ties occur, the label which is better on average w.r.t. costs over all training data is
#' preferred.)
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
  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  feats = .task$env$data
  costs = .task$env$costs
  cns = colnames(costs)
  # compute average costs of all classes, then sort labels by it
  cns.costs = colSums(costs)
  cns = cns[order(cns.costs, decreasing = FALSE)]
  costs = costs[, cns]
  # case of equals best costs, take the one which is better on whole data
  newy = getMinIndexOfRows(costs, ties.method = "first")
  newy = cns[newy]
  # if all equal, predict one class, stupid fringe case
  if (length(unique(newy)) == 1) {
    model = newy[1]
  } else {
    data = cbind(feats, ..y.. = newy)
    task = makeClassifTask(data = data, target = "..y..",
      check.data = FALSE, fixup.data = "quiet")
    model = train(.learner$next.learner, task)
  }
  makeChainModel(next.model = model, cl = "CostSensClassifModel")
}

#' @S3method predictLearner CostSensClassifWrapper
predictLearner.CostSensClassifWrapper = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model$next.model
  # handle constant prediction
  if (is.character(m))
    return(as.factor(rep(m, nrow(.newdata))))
  NextMethod()
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


