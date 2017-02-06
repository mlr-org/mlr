#' @title Learner for knn on functional data
#'
#' @description Learner for knn on functional data
#'
#' @export
makeRLearner.fdaclassif.knn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.knn",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "knn", lower = 1),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = list("metric.lp", "metric.kl", "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = list("GCV.S", "CV.S", "GCCV.S")),
      makeUntypedLearnerParam(id = "par.CV"),
      makeUntypedLearnerParam(id = "par.S")
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Knn on FDA",
    short.name = "knnFDA"
  )
}

#' @export
trainLearner.fdaclassif.knn = function(.learner, .task, .subset, .weights = NULL, ...) {

  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  data.fdclass = fdata(mdata = z$data)
  glearn = z$target

  learned.model = classif.knn(group = glearn, fdataobj = data.fdclass,...)

  return(learned.model)
}

#' @export
predictLearner.fdaclassif.knn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd.fdclass = fdata(mdata = .newdata)
  class.pred = predict(object = m, new.fdataobf = nd, ...)

  return(class.pred)
}


