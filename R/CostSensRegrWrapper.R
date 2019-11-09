#' @title Wraps a regression learner for use in cost-sensitive learning.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via [getLearnerModel].
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
  learner = checkLearner(learner, "regr")
  # we cannot make use of 'se' here
  learner = setPredictType(learner, "response")
  id = stri_paste("costsens", learner$id, sep = ".")
  makeHomogeneousEnsemble(id, type = "costsens", learner, package = learner$package,
    learner.subclass = "CostSensRegrWrapper", model.subclass = "CostSensRegrModel")
}

#' @export
trainLearner.CostSensRegrWrapper = function(.learner, .task, .subset = NULL, ...) {
  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  d = getTaskData(.task)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doCostSensRegrTrainIteration,
    class.levels = getTaskDesc(.task)$class.levels, more.args = list("d" = d,
      "costs" = getTaskCosts(.task), "learner" = .learner), level = "mlr.ensemble")
  makeHomChainModel(.learner, models)
}

doCostSensRegrTrainIteration = function(learner, class.levels, costs, d) {
  setSlaveOptions()
  data = cbind(d, ..y.. = costs[, class.levels])
  task = makeRegrTask(id = class.levels, data = data, target = "..y..", check.data = FALSE, fixup.data = "quiet")
  train(learner$next.learner, task)
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
getLearnerProperties.CostSensRegrWrapper = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), c("weights", "prob"))
}
