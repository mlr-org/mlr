#' @export
makeRLearner.classif.liquidSVM = function() {
  makeRLearnerClassif(
    cl = "classif.liquidSVM",
    package = "liquidSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "d", lower = 0L),
      makeLogicalLearnerParam(id = "scale"),
      makeIntegerLearnerParam(id = "threads", lower = -1L),
      makeIntegerLearnerParam(id = "partition_choice", lower = 0L),
      makeIntegerLearnerParam(id = "grid_choice", lower = -2L, upper = 2L),
      makeIntegerLearnerParam(id = "adaptivity_control", lower = 0L, upper = 2L),
      makeIntegerLearnerParam(id = "random_seed", default = 1),
      makeIntegerLearnerParam(id = "fold"),
      makeIntegerLearnerParam(id = "clipping"),
      makeIntegerLearnerParam(id = "gamma_steps", lower = 0),
      makeNumericLearnerParam(id = "min_gamma", lower = 0),
      makeNumericLearnerParam(id = "max_gamma", lower = 0),
      makeNumericVectorLearnerParam(id = "gammas", lower = 0),
      makeIntegerLearnerParam(id = "lambda_steps", lower = 0),
      makeNumericLearnerParam(id = "min_lambda", lower = 0),
      makeNumericLearnerParam(id = "max_lambda", lower = 0),
      makeNumericVectorLearnerParam(id = "lambdas", lower = 0),
      makeNumericVectorLearnerParam(id = "c_values", lower = 0),
      makeLogicalLearnerParam(id = "useCells")
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors"),
    name = "Support Vector Machines (liquidSVM)",
    short.name = "liquidSVM",
    callees = "liquidSVM"
  )
}

#' @export
trainLearner.classif.liquidSVM = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  liquidSVM::svm(f, getTaskData(.task, .subset),  ...)
  #liquidSVM::svm(f, getTaskData(.task, .subset), predict.prob = .learner$predict.type == "prob",  ...)
}

#' @export
predictLearner.classif.liquidSVM = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
  # res = as.matrix(predict(.model$learner.model, newdata = .newdata, ...))
  # res = res/rowSums(res)
  # colnames(res) = getTaskClassLevels(.task)
  # rownames(res) = NULL
  # res
}
