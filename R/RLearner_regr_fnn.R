#' @export
makeRLearner.regr.fnn = function() {
  makeRLearnerRegr(
    cl = "regr.fnn",
    package = "FNN",
    # l is for reject option. cannot be done with mlr atm
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeLogicalLearnerParam(id = "use.all", default = TRUE, requires = expression(algorithm == "VR")),
      makeDiscreteLearnerParam(id = "algorithm", default = "cover_tree", values = list("cover_tree", "kd_tree", "VR"))
    ),
    properties = c("numerics"),
    name = "regr.fnn",
    short.name = "fnn",
    note = ""
  )
}

#' @export
trainLearner.regr.fnn = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  list(train = d, parset = list(...))
}

#' @export
predictLearner.regr.fnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  pars = c(list(train = m$train$data, test = .newdata, y = m$train$target), m$parset, list(...))
  do.call(FNN::knn.reg, pars)$pred
}
