#' @export
makeRLearner.classif.cvglinternet = function() {
  makeRLearnerClassif(
    cl = "classif.cvglinternet",
    package = "glinternet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nFolds", default = 10L, lower = 1L, tunable = FALSE),    
      makeNumericVectorLearnerParam(id = "lambda", lower = 0),
      makeIntegerLearnerParam(id = "nLambda", default = 50L, lower = 1L),
      makeNumericLearnerParam(id = "lambdaMinRatio", lower = 0, upper = 1, default = 0.01),
      makeIntegerLearnerParam(id = "screenLimit", lower = 1L),
      makeNumericLearnerParam(id = "tol", default = 1.0e-5, lower = 0),
      makeIntegerLearnerParam(id = "maxIter", default = 5000L, lower = 1L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "lambdaType", default = "lambdaHat", values = c("lambdaHat", "lambdaHat1Std"), when = "predict")
    ),
    properties = c("numerics", "factors", "ordered", "prob", "twoclass"),
    name = "Linear Interaction Model with Group-Lasso Regularization (Cross Validated Lambda)",
    short.name = "cvglinternet",
    note = "(Ordered) factor features are automatically converted to integers."
  )
}

#' @export
trainLearner.classif.cvglinternet = function(.learner, .task, .subset, .weights = NULL, ...) {
  ## class labels must be 0, 1
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  ## categorical variables must be coded as integers starting with 0
  info = getFixDataInfo(d$data, factors.to.int = TRUE, ordered.to.int = TRUE, min.int.zero = TRUE)
  ## numLevels contains the number of levels for categorical features, continuous features get a 1
  numLevels = sapply(d$data, nlevels)
  numLevels[numLevels == 0] = 1
  attachTrainingInfo(glinternet::glinternet.cv(X = as.matrix(fixDataForLearner(d$data, info)), Y = d$target,
    numLevels = numLevels, family = "binomial", ...), info)
}

#' @export
predictLearner.classif.cvglinternet = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  p = predict(.model$learner.model, X = .newdata, type = "response", ...)
  if (.learner$predict.type == "response") {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
  } else {
    p = propVectorToMatrix(p, levs)
  }
  p
}
