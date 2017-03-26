#' @export
makeRLearner.fdaclassif.classiKnn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.classiKnn",
    package = "classiFunc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn", lower = 1L, default = 1L),
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
                              lower = 1L),
      # TODO add upper = expression(getTaskData(.task))
      # TODO add analogously dmay, dmin1, dmin2, dmax1, dmax2, t1, t2
      # TODO add .poi
      # additional arguments to metrics in computeDistMat
      makeNumericLearnerParam(id = "p", default = 2)
      # additional arguments to Data2fd

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
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  learned.model = do.call(classiFunc::classiKnn, c(list(classes = z$target,
                                                          fdata = z$data),
                                                     getLearnerParVals(.learner)))
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.classiKnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  predict(m, newdata = .newdata, predict.type = .learner$predict.type, ...)
}
