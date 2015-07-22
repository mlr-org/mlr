#' @title Wraps a regression learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
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
  makeHomogeneousEnsemble(id, type = "costsens", learner, package = learner$package,
    learner.subclass = "CostSensRegrWrapper", model.subclass = "CostSensRegrModel")
}

#' @export
trainLearner.CostSensRegrWrapper = function(.learner, .task, .subset, ...) {
  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  costs = getTaskCosts(.task)
  td = getTaskDescription(.task)
  classes = td$class.levels
  models = vector("list", length = length(classes))
  for (i in seq_along(classes)) {
    cl = classes[i]
    y = costs[, cl]
    data = cbind(getTaskData(.task), ..y.. = y)
    task = makeRegrTask(id = cl, data = data, target = "..y..",
      check.data = FALSE, fixup.data = "quiet")
    models[[i]] = train(.learner$next.learner, task)
  }
  m = makeHomChainModel(.learner, models)
}

#' @export
predictLearner.CostSensRegrWrapper = function(.learner, .model, .newdata, ...) {
  p = predictHomogeneousEnsemble(.learner, .model, .newdata, ...)
  # get class per row with minimal estimated costs
  p = apply(p, 1L, getMinIndex)
  classes = .model$task.desc$class.levels
  factor(classes[p], levels = classes)
}


#' @export
getLearnerProperties = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), c("weights", "prob"))
}
