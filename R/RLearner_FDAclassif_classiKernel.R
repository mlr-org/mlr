#' @export
makeRLearner.fdaclassif.classiKernel = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.classiKernel",
    package = "classiFunc",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "h",
                              lower = 0, upper = Inf,
                              default = 1),
      # FIXME: is it ok to hand over the values with this function
      # from the original package?
      makeDiscreteLearnerParam(id = "metric", default = "Euclidean",
                               values = classiFunc::metricChoices()),
      makeDiscreteLearnerParam(id = "ker", default = "Ker.norm",
                               values = classiFunc::kerChoices()),
      makeIntegerLearnerParam(id = "nderiv", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "derived", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "deriv.method", default = "base.diff",
                               values = c("base.diff", "fda.deriv.fd")),
      makeFunctionLearnerParam(id = "custom.metric",
                               default = function(x, y, ...) {
                                 return(sqrt(sum( (x - y) ^ 2)))
                                 },
                               requires = quote(metric == "custom.metric"),
                               tunable = FALSE),
      makeFunctionLearnerParam(id = "custom.ker",
                               default = function(u) {
                                 return(dnorm(u))
                               },
                               requires = quote(ker == "custom.ker"),
                               tunable = FALSE),
      # additional arguments to computeDistMat
      makeNumericLearnerParam(id = "dmin", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin1", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax1", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin2", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax2", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t1", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t2", default = 1,
                              lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = ".poi", # default = expression(seq(0, 1, ncol(task))),
                                    lower = 0, upper = 1),
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
trainLearner.fdaclassif.classiKernel = function(.learner, .task, .subset, ...) {
  task.desc = getTaskDesc(.task)
  if (length(task.desc$fd.features) > 1) {
    stop("This learner can only be used for data with one functional covariable.")
  }
  fd.features = unlist(task.desc$fd.features)
  grid = unlist(task.desc$fd.grids, use.names = FALSE)

  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  .learner$par.set = evaluateParamExpressions(.learner$par.set,
                                              dict = list(task = .task))
  learned.model = do.call(classiFunc::classiKernel,
                          c(list(classes = z$target,
                                 fdata = z$data[, fd.features],
                                 grid = grid),
                            getLearnerParVals(.learner)))
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.classiKernel = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  predict(m, newdata = .newdata, predict.type = .learner$predict.type, ...)
}
