# learner with error "foo" in predict
makeRLearner.classif.__mlrmocklearners__1 = function() {
  makeRLearnerClassif(
    cl = "classif.__mlrmocklearners__1", package = character(0L), par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.__mlrmocklearners__1 = function(.learner, .task, .subset, .weights = NULL,  ...) list()
predictLearner.classif.__mlrmocklearners__1 = function(.learner, .model, .newdata, ...) stop("foo")
registerS3method("makeRLearner", "classif.__mlrmocklearners__1", makeRLearner.classif.__mlrmocklearners__1)
registerS3method("trainLearner", "classif.__mlrmocklearners__1", trainLearner.classif.__mlrmocklearners__1)
registerS3method("predictLearner", "classif.__mlrmocklearners__1", predictLearner.classif.__mlrmocklearners__1)

# for tuning, produces errors en masse
makeRLearner.classif.__mlrmocklearners__2 = function() {
  makeRLearnerClassif(
    cl = "classif.__mlrmocklearners__2", package = character(0L),
    par.set = makeParamSet(
      makeNumericLearnerParam("alpha", lower = 0, upper = 1)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.__mlrmocklearners__2 = function(.learner, .task, .subset, .weights = NULL, alpha, ...) {
  if (alpha < 0.5)
    stop("foo")
  list()
}
predictLearner.classif.__mlrmocklearners__2 = function(.learner, .model, .newdata, ...) {
  as.factor(sample(.model$task.desc$class.levels, nrow(.newdata), replace = TRUE))
}
registerS3method("makeRLearner", "classif.__mlrmocklearners__2", makeRLearner.classif.__mlrmocklearners__2)
registerS3method("trainLearner", "classif.__mlrmocklearners__2", trainLearner.classif.__mlrmocklearners__2)
registerS3method("predictLearner", "classif.__mlrmocklearners__2", predictLearner.classif.__mlrmocklearners__2)


# learner with error "foo" in train
makeRLearner.classif.__mlrmocklearners__3 = function() {
  makeRLearnerClassif(
    cl = "classif.__mlrmocklearners__3", package = character(0L), par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}
trainLearner.classif.__mlrmocklearners__3 = function(.learner, .task, .subset, .weights = NULL,  ...) stop("foo")
predictLearner.classif.__mlrmocklearners__3 = function(.learner, .model, .newdata, ...) 1L
registerS3method("makeRLearner", "classif.__mlrmocklearners__3", makeRLearner.classif.__mlrmocklearners__3)
registerS3method("trainLearner", "classif.__mlrmocklearners__3", trainLearner.classif.__mlrmocklearners__3)
registerS3method("predictLearner", "classif.__mlrmocklearners__3", predictLearner.classif.__mlrmocklearners__3)

# learner with different "when" settings for hyperpars
makeRLearner.regr.__mlrmocklearners__4 = function() {
  makeRLearnerRegr(
    cl = "regr.__mlrmocklearners__4", package = character(0L),
    par.set = makeParamSet(
      makeNumericLearnerParam("p1", when = "train"),
      makeNumericLearnerParam("p2", when = "predict"),
      makeNumericLearnerParam("p3", when = "both")
    ),
    properties = c("missings", "numerics", "factors")
  )
}

trainLearner.regr.__mlrmocklearners__4 = function(.learner, .task, .subset, .weights = NULL, p1, p3, ...) {
  list(foo = p1 + p3)
}

predictLearner.regr.__mlrmocklearners__4 = function(.learner, .model, .newdata, p2, p3) {
  y = rep(1, nrow(.newdata))
  y * .model$learner.model$foo + p2 + p3
}
registerS3method("makeRLearner", "regr.__mlrmocklearners__4", makeRLearner.regr.__mlrmocklearners__4)
registerS3method("trainLearner", "regr.__mlrmocklearners__4", trainLearner.regr.__mlrmocklearners__4)
registerS3method("predictLearner", "regr.__mlrmocklearners__4", predictLearner.regr.__mlrmocklearners__4)


# Learner cannot use expression in param requires
makeRLearner.classif.__mlrmocklearners__5 = function() {
  makeRLearnerClassif(
    cl = "classif.__mlrmocklearners__5",
    package = "mlr",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "a", values = c("x", "y")),
      makeNumericLearnerParam(id = "b", requires = expression(a == "x"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob")
  )
}
registerS3method("makeRLearner", "classif.__mlrmocklearners__5", makeRLearner.classif.__mlrmocklearners__5)

