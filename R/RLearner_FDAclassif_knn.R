#' @export
makeRLearner.fdaclassif.knn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.knn",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn", lower = 1L, default = NULL, special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = c("metric.lp", "metric.kl",
        "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = TRUE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Knn on FDA",
    short.name = "knnFDA"
  )
}

#' @export
trainLearner.fdaclassif.knn = function(.learner, .task, .subset, .weights = NULL, trim, draw, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = z$data)
  par.CV = learnerArgsToControl(list, trim, draw)
  par.S = list(w = .weights)
  glearn = z$target
  learned.model = fda.usc::classif.knn(group = glearn, fdataobj = data.fdclass,
    par.CV = par.CV, par.S = par.S, ...)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.knn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd.fdclass = fda.usc::fdata(mdata = .newdata)# transform the data into fda.usc:fdata class type.
  class.pred = predict(m, nd.fdclass, ...)
  return(class.pred)
}
