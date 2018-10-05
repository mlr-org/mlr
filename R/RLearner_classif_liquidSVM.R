#' @export
makeRLearner.classif.liquidSVM = function() {
  makeRLearnerClassif(
    cl = "classif.liquidSVM",
    package = "liquidSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "d", lower = 0L, upper = 7L, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale", default = TRUE),
      makeIntegerLearnerParam(id = "threads", lower = -1L, default = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "gauss_rbf", values = c("gauss_rbf","poisson")),
      makeIntegerLearnerParam(id = "partition_choice", lower = 0L, upper = 6L, default = 0),
      makeIntegerLearnerParam(id = "grid_choice", lower = -2L, upper = 2L),
      makeIntegerLearnerParam(id = "adaptivity_control", lower = 0L, upper = 2L, default = 0),
      makeIntegerLearnerParam(id = "random_seed"),
      makeIntegerLearnerParam(id = "folds", lower = 1, tunable = FALSE),
      makeIntegerLearnerParam(id = "clipping", lower = -1),
      makeIntegerLearnerParam(id = "gamma_steps", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas))),
      makeNumericLearnerParam(id = "min_gamma", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas) & min_gamma <= max_gamma)),
      makeNumericLearnerParam(id = "max_gamma", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas) & min_gamma <= max_gamma)),
      makeNumericVectorLearnerParam(id = "gammas", lower = 0),
      makeIntegerLearnerParam(id = "lambda_steps", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas))),
      makeNumericLearnerParam(id = "min_lambda", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas) & min_lambda <= max_lambda)),
      makeNumericLearnerParam(id = "max_lambda", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas) & min_lambda <= max_lambda)),
      makeNumericVectorLearnerParam(id = "lambdas", lower = 0),
      makeNumericVectorLearnerParam(id = "c_values", lower = 0),
      makeLogicalLearnerParam(id = "useCells", default = FALSE)
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
