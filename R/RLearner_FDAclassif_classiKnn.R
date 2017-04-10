#' @export
makeRLearner.fdaclassif.classiKnn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.classiKnn",
    package = "classiFunc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn",
                              lower = 1L, upper = expression(0.5 * nrow(getTaskData(task))),
                              default = 1L),
      # FIXME: is it ok to hand over the values with this function
      # from the original package?
      makeDiscreteLearnerParam(id = "metric", default = "Euclidean",
                               values = classiFunc::metric.choices()),
      makeIntegerLearnerParam(id = "nderiv", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "derived", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "deriv.method", default = "base.diff",
                               values = c("base.diff", "fda.deriv.fd")),
      makeFunctionLearnerParam(id = "custom.metric",
                               default = function(x, y, ...) {
                                 return(sqrt(sum((x - y)^2)))},
                               requires = quote(metric == "custom.metric"),
                               tunable = FALSE),
      # additional arguments to computeDistMat
      makeIntegerLearnerParam(id = "dmin", default = 1L,
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "dmax", default = expression(ncol(getTaskData(task)) - 1),
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "dmin1", default = 1L,
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "dmax1", default = expression(ncol(getTaskData(task)) - 1),
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "dmin2", default = 1L,
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "dmax2", default = expression(ncol(getTaskData(task)) - 1),
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "t1", default = 1L,
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerLearnerParam(id = "t2", default = expression(ncol(getTaskData(task)) - 1),
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      makeIntegerVectorLearnerParam(id = ".poi", default = expression(1:(ncol(getTaskData(task)) - 1)),
                              lower = 1L, upper = expression(ncol(getTaskData(task)) - 1)),
      # additional arguments to metrics in computeDistMat
      makeNumericLearnerParam(id = "p", default = 2),
      # TODO additional arguments to Data2fd
      # TODO additional arguments to custom metric
      keys = c("task"),
      forbidden = expression(dmin > dmax,
                             knn %% 2 == 0)
    ),
    # par.vals = list(metric = "Euclidean", knn = 1L),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "classiKnn on FDA",
    short.name = "classiKnnFDA",
    note = ""
  )
}


#' @export
trainLearner.fdaclassif.classiKnn = function(.learner, .task, .subset, ...) {
  task.desc = getTaskDesc(.task)
  if(length(task.desc$fd.features) > 1) {
    stop("This learner can only be used for data with one functional covariable.")
  }
  fd.features = unlist(task.desc$fd.features)
  grid = unlist(task.desc$fd.grids, use.names = FALSE)

  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  .learner$par.set = evaluateParamExpressions(.learner$par.set, dict = list(task = .task))
  learned.model = do.call(classiFunc::classiKnn, c(list(classes = z$target,
                                                        fdata = z$data[,fd.features],
                                                        grid = grid),
                                                   getLearnerParVals(.learner)))
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.classiKnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  predict(m, newdata = .newdata, predict.type = .learner$predict.type, ...)
}
