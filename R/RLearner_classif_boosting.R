# FIXME: interface was changed, read page, pars, maybe rename
#' @export
makeRLearner.classif.boosting = function() {
  makeRLearnerClassif(
    cl = "classif.boosting",
    package = "adabag",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "boos", default = TRUE),
      makeIntegerLearnerParam(id = "mfinal", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "coeflearn", default = "Breiman", values = c("Breiman", "Freund")),
      # rpart.control arguments
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
      makeIntegerLearnerParam(id = "xval", default = 0L, lower = 0L)
    ),
    par.vals = list(xval = 0L),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}

#' @export
trainLearner.classif.boosting= function(.learner, .task, .subset, .weights = NULL, minsplit, minbucket, cp, maxcompete, maxsurrogate, usesurrogate, surrogatestyle, maxdepth, xval, ...) {
  f = getTaskFormula(.task)
  ctrl = learnerArgsToControl(rpart.control, minsplit, minbucket, cp, maxcompete, maxsurrogate, usesurrogate, surrogatestyle, maxdepth, xval)
  boosting(f, data = getTaskData(.task, .subset), control = ctrl, ...)
}

#' @export
predictLearner.classif.boosting = function(.learner, .model, .newdata, ...) {
  levs = levels = .model$task.desc$class.levels
  # stupid adaboost
  .newdata[, .model$task.desc$target] = factor(rep(1, nrow(.newdata)), levels = levs)
  p = predict(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "prob") {
    return(setColNames(p$prob, levs))
  } else {
    return(as.factor(p$class))
  }
}
