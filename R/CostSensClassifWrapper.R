#' @title Wraps a classification learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' The classification model can easily be accessed via [getLearnerModel].
#'
#' This is a very naive learner, where the costs are transformed into classification labels -
#' the label for each case is the name of class with minimal costs.
#' (If ties occur, the label which is better on average w.r.t. costs over all training data is
#' preferred.)
#' Then the classifier is fitted to that data and subsequently used for prediction.
#'
#' @template arg_learner_classif
#' @template ret_learner
#' @export
#' @family costsens
#' @family wrapper
#' @aliases CostSensClassifWrapper CostSensClassifModel
makeCostSensClassifWrapper = function(learner) {
  learner = checkLearner(learner, "classif")
  learner = setPredictType(learner, "response")
  id = stri_paste("costsens", learner$id, sep = ".")
  makeBaseWrapper(id, "costsens", learner, package = learner$package,
    learner.subclass = "CostSensClassifWrapper", model.subclass = "CostSensClassifModel")
}

#' @export
trainLearner.CostSensClassifWrapper = function(.learner, .task, .subset = NULL, ...) {

  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  feats = getTaskData(.task)
  costs = getTaskCosts(.task)
  cns = colnames(costs)
  # compute average costs of all classes, then sort labels by it
  cns.costs = colSums(costs)
  cns = cns[order(cns.costs, decreasing = FALSE)]
  costs = costs[, cns, drop = FALSE]
  # case of equals best costs, take the one which is better on whole data
  newy = getMinIndexOfRows(costs, ties.method = "first")
  newy = cns[newy]
  # if all equal, predict one class, stupid fringe case
  if (length(unique(newy)) == 1) {
    m = makeS3Obj("CostSensClassifModelConstant", y = newy[1L])
    model = makeWrappedModel.Learner(.learner, m, getTaskDesc(.task), .subset, getTaskFeatureNames(.task),
      getTaskFactorLevels(.task), 0)
  } else {
    data = cbind(feats, ..y.. = newy)
    task = makeClassifTask(data = data, target = "..y..",
      check.data = FALSE, fixup.data = "quiet")
    model = train(.learner$next.learner, task)
  }
  makeChainModel(next.model = model, cl = "CostSensClassifModel")
}

#' @export
predictLearner.CostSensClassifWrapper = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model$next.model
  mm = m$learner.model
  # handle constant prediction
  if (inherits(mm, "CostSensClassifModelConstant")) {
    return(as.factor(rep(mm$y, nrow(.newdata))))
  }
  NextMethod()
}

#' @export
getLearnerProperties.CostSensClassifWrapper = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), c("weights", "prob"))
}
