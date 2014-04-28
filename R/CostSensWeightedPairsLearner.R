#' @title Wraps a regression learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via \code{\link{getCostSensWeightedPairsModels}}.
#'
#' For each class in the task, an individual regression model is fitted for the costs of that class.
#' During prediction, the class with the lowest predicted costs is selected.
#'
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   The basic regression learner.
#' @return [\code{\link[mlr]{Learner}}].
#' @export
makeCostSensWeightedPairsWrapper = function(learner) {
  checkLearnerClassif(learner, weights = TRUE)
  learner = setPredictType(learner, "response")
  id = paste("costsens", learner$id, sep = ".")
  x = makeBaseWrapper(id, learner, package = learner$packages, cl = "CostSensWeightedPairsWrapper")
  x$type = "costsens"
  x$weights = FALSE
  x$se = FALSE
  x$prob = FALSE
  x
}

#' @S3method trainLearner CostSensWeightedPairsWrapper
trainLearner.CostSensWeightedPairsWrapper = function(.learner, .task, .subset, ...) {
  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  costs = .task$env$costs
  classes = .task$task.desc$class.levels
  k = length(classes)
  feats = .task$env$data
  models = vector("list", length = k * (k - 1) / 2)

  counter = 1
  for (i in 1:(k-1)) {
    a1 = classes[i]
    for (j in (i+1):k) {
      a2 = classes[j]
      y = ifelse(costs[, a1] < costs[, a2], a1, a2)
      # if on the sample one alg is always better, always predict it
      # FIXME: really need this?
      if (all(y == a1) || all(y == a2)) {
        models[[counter]] = y[1]
      } else {
        feats$..y.. = y
        task = makeClassifTask(data = feats, target = "..y..")
        w = abs(costs[, a1] - costs[, a2])
        models[[counter]] = train(.learner$next.learner, task, weights = w)
      }
      counter = counter + 1L
    }
  }
  makeChainModel(next.model = models, cl = "CostSensWeightedPairsModel")
}


#' @S3method predictLearner CostSensWeightedPairsWrapper
predictLearner.CostSensWeightedPairsWrapper = function(.learner, .model, .newdata, ...) {
  classes = .model$task.desc$class.levels
  models = getCostSensWeightedPairsModels(.model)
  preds = sapply(models, function(mod) {
    n = nrow(.newdata)
    # FIXME: really need this?
    if (is.character(mod))
       rep(mod, n)
    else
      as.character(predict(mod, newdata = .newdata, ...)$data$response)
  })
  factor(apply(preds, 1, computeMode), levels = classes)
}


#' @S3method makeWrappedModel CostSensWeightedPairsWrapper
makeWrappedModel.CostSensWeightedPairsWrapper = function(learner, model, task.desc, subset, features,
  factor.levels, time) {

  x = NextMethod()
  class(x) = c("CostSensWeightedPairsModel", class(x))
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
getCostSensWeightedPairsModels = function(model, learner.models = FALSE) {
  checkArg(model, "CostSensWeightedPairsModel")
  ms = model$learner.model$next.model
  if (learner.models)
    extractSubList(ms, "learner.model", simplify = FALSE)
  else
    ms
}

