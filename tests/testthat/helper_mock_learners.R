# learner with error "foo" in predict
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


# learner with error "foo" in train
makeRLearner.classif.mock3 = function() {
  makeRLearnerClassif(
    cl = "classif.mock3", package = character(0L), par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.mock3 = function(.learner, .task, .subset, .weights = NULL,  ...) stop("foo")
predictLearner.classif.mock3 = function(.learner, .model, .newdata, ...) 1L
registerS3method("makeRLearner", "classif.mock3", makeRLearner.classif.mock3)
registerS3method("trainLearner", "classif.mock3", trainLearner.classif.mock3)
registerS3method("predictLearner", "classif.mock3", predictLearner.classif.mock3)

# learner with different "when" settings for hyperpars
makeRLearner.regr.mock4 = function() {
  makeRLearnerRegr(
    cl = "regr.mock4", package = character(0L),
    par.set = makeParamSet(
      makeNumericLearnerParam("p1", when = "train"),
      makeNumericLearnerParam("p2", when = "predict"),
      makeNumericLearnerParam("p3", when = "both")
    ),
    properties = c("missings", "numerics", "factors")
  )
}

trainLearner.regr.mock4 = function(.learner, .task, .subset, .weights = NULL, p1, p3, ...) {
  list(foo = p1 + p3)
}

predictLearner.regr.mock4 = function(.learner, .model, .newdata, p2, p3) {
  y = rep(1, nrow(.newdata))
  y * .model$learner.model$foo + p2 + p3
}
registerS3method("makeRLearner", "regr.mock4", makeRLearner.regr.mock4)
registerS3method("trainLearner", "regr.mock4", trainLearner.regr.mock4)
registerS3method("predictLearner", "regr.mock4", predictLearner.regr.mock4)
