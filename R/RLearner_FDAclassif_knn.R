#' @export
makeRLearner.fdaclassif.knn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.knn",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn", lower = 1L, default = NULL, special.vals = list(NULL)),
      # metric.kl throws (cryptic) warning messages
      # metric.dist does not work in original fda.usc package
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp",
                               values = c("metric.lp",
                                          # "metric.kl", "metric.dist",
                                          "metric.hausdorff")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S",
                               values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "lp", default = 2L, lower = 1L, upper = Inf,
                              requires = quote("metric" == "metric.lp"))
    ),
    par.vals = list(draw = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "weights"),
    name = "Knn on FDA",
    short.name = "knnFDA",
    note = "Draw parameter is set to FALSE as default."
  )
}


#' @export
trainLearner.fdaclassif.knn = function(.learner, .task, .subset, .weights = NULL, metric, trim, draw, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = z$data)
  par.cv = learnerArgsToControl(list, trim, draw)
  par.s = list(w = .weights)
  glearn = z$target
  metric = switch(metric,
                  metric.lp = fda.usc::metric.lp,
                  # metric.kl = fda.usc::metric.kl,
                  # metric.dist = fda.usc::metric.dist,
                  metric.hausdorff = fda.usc::metric.hausdorff
  )
  learned.model = fda.usc::classif.knn(group = glearn, fdataobj = data.fdclass,
                                       par.CV = par.cv, par.S = par.s,
                                       metric = metric, ...)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.knn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd.fdclass = fda.usc::fdata(mdata = .newdata)# transform the data into fda.usc:fdata class type.
  class.pred = predict(m, nd.fdclass, ...)
  return(class.pred)
}
