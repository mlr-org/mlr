makeRLearner.classif.mock = function() {
  makeRLearnerClassif(
    cl = "classif.mock", package = character(0L), par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.mock = function(.learner, .task, .subset, .weights = NULL,  ...) list()
predictLearner.classif.mock = function(.learner, .model, .newdata, ...) stop("foo")
registerS3method("makeRLearner", "classif.mock", makeRLearner.classif.mock)
registerS3method("trainLearner", "classif.mock", trainLearner.classif.mock)
registerS3method("predictLearner", "classif.mock", predictLearner.classif.mock)

