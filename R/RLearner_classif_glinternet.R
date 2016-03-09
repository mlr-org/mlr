#' @export
makeRLearner.classif.glinternet = function() {
  makeRLearnerClassif(
    cl = "classif.glinternet",
    package = "glinternet",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "lambda", lower = 0),
      makeIntegerLearnerParam(id = "nLambda", default = 50L, lower = 1L),
      makeNumericLearnerParam(id = "lambdaMinRatio", lower = 0, upper = 1, default = 0.01),
      makeIntegerLearnerParam(id = "screenLimit", lower = 1L),
      makeIntegerLearnerParam(id = "numToFind", lower = 1L),
      makeNumericLearnerParam(id = "tol", default = 1.0e-5, lower = 0),
      makeIntegerLearnerParam(id = "maxIter", default = 5000L, lower = 1L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeNumericVectorLearnerParam(id = "predictLambda", when = "predict")
    ),
    properties = c("numerics", "factors", "ordered", "prob", "twoclass"),
    par.vals = list(predictLambda = 0.01),
    name = "Linear Interaction Model with Group-Lasso Regularization",
    short.name = "glinternet",
    note = 'Learner param `predictLambda` can only be a single number, maps to `lambda` in `predict.glinternet` and
      has been set to `0.01` per default.
      If `predictLambda` is not a member of the `lambda` sequence used in training it is rounded to the next possible value.
      (Ordered) factor features are automatically converted to integers.'
  )
}

#' @export
trainLearner.classif.glinternet = function(.learner, .task, .subset, .weights = NULL, ...) {
  ## class labels must be 0, 1
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  ## categorical variables must be coded as integers starting with 0
  info = getFixDataInfo(d$data, factors.to.int = TRUE, ordered.to.int = TRUE, min.int.zero = TRUE)
  ## numLevels contains the number of levels for categorical features, continuous features get a 1
  numLevels = sapply(d$data, nlevels)
  numLevels[numLevels == 0] = 1
  attachTrainingInfo(glinternet::glinternet(X = as.matrix(fixDataForLearner(d$data, info)), Y = d$target,
    numLevels = numLevels, family = "binomial", ...), info)
}

#' @export
predictLearner.classif.glinternet = function(.learner, .model, .newdata, predictLambda, ...) {
  ## map predictLambda to nearest value in the lambda sequence (FIXME there might be a better solution ...)
  lambda = .model$learner.model$lambda
  lambda = lambda[which.min(abs(lambda - predictLambda))]
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  p = predict(.model$learner.model, X = .newdata, type = "response", lambda = lambda, ...)
  if (.learner$predict.type == "response") {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
  } else {
    p = propVectorToMatrix(p, levs)
  }
  p
}
