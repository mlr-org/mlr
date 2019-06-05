#' @export
makeRLearner.classif.ada = function() {
  makeRLearnerClassif(
    cl = "classif.ada",
    package = c("ada", "rpart"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "loss", default = "exponential", values = c("exponential", "logistic")),
      makeDiscreteLearnerParam(id = "type", default = "discrete", values = c("discrete", "real", "gentle")),
      makeIntegerLearnerParam(id = "iter", default = 50L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0),
      makeNumericLearnerParam(id = "bag.frac", default = 0.5, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "model.coef", default = TRUE),
      makeLogicalLearnerParam(id = "bag.shift", default = FALSE),
      makeIntegerLearnerParam(id = "max.iter", default = 20L, lower = 1L),
      makeNumericLearnerParam(id = "delta", default = 1e-10, lower = 0),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
      makeIntegerLearnerParam(id = "xval", default = 10L, lower = 0L, tunable = FALSE)
    ),
    par.vals = list(xval = 0L),
    properties = c("twoclass", "numerics", "factors", "prob"),
    name = "ada Boosting",
    short.name = "ada",
    note = "`xval` has been set to `0` by default for speed.",
    callees = c("ada", "rpart.control")
  )
}

#' @export
trainLearner.classif.ada = function(.learner, .task, .subset, .weights = NULL, ...) {

  f = getTaskFormula(.task)
  dots = list(...)
  # get names of rpart.control args
  ctrl.names = names(formals(rpart::rpart.control))
  # subset args into contrl.args and all other args (dots)
  ctrl.args = dots[intersect(names(dots), ctrl.names)]
  dots = dropNamed(dots, ctrl.names)
  # execute ada with proper args
  ada.args = c(dots, control = list(ctrl.args))
  ada.fun = function(...) {
    ada::ada(f, getTaskData(.task, .subset), ...)
  }
  do.call(ada.fun, ada.args)
}

#' @export
predictLearner.classif.ada = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "vector", "probs")
  mod = getLearnerModel(.model)
  p = predict(mod, newdata = .newdata, type = type, ...)
  if (type == "probs") {
    colnames(p) = rownames(mod$confusion)
  }
  return(p)
}
