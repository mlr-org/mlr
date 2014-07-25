makeRLearner.classif.mock1 = function() {
  makeRLearnerClassif(
    cl = "classif.mock1", package = character(0L), par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.mock1 = function(.learner, .task, .subset, .weights = NULL,  ...) list()
predictLearner.classif.mock1 = function(.learner, .model, .newdata, ...) stop("foo")
registerS3method("makeRLearner", "classif.mock1", makeRLearner.classif.mock1)
registerS3method("trainLearner", "classif.mock1", trainLearner.classif.mock1)
registerS3method("predictLearner", "classif.mock1", predictLearner.classif.mock1)

# for tuning, produces errors en masse
makeRLearner.classif.mock2 = function() {
  makeRLearnerClassif(
    cl = "classif.mock2", package = character(0L),
    par.set = makeParamSet(
      makeNumericLearnerParam("alpha", lower = 0, upper = 1)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.mock2 = function(.learner, .task, .subset, .weights = NULL, alpha, ...) {
  if (alpha < 0.5)
    stop("foo")
  list()
}
predictLearner.classif.mock2 = function(.learner, .model, .newdata, ...) {
  as.factor(sample(.model$task.desc$class.levels, nrow(.newdata), replace = TRUE))
}
registerS3method("makeRLearner", "classif.mock2", makeRLearner.classif.mock2)
registerS3method("trainLearner", "classif.mock2", trainLearner.classif.mock2)
registerS3method("predictLearner", "classif.mock2", predictLearner.classif.mock2)


