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
    par.vals = list(draw = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "weights", "prob"),
    name = "fdaknn",
    short.name = "fdaknn",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.fdaclassif.knn = function(.learner, .task, .subset, .weights = NULL, trim, draw, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = z$data)
  par.cv = learnerArgsToControl(list, trim, draw)
  fda.usc::classif.knn(group = z$target, fdataobj = data.fdclass, par.CV = par.cv,
    par.S = list(w = .weights), ...)
}

#' @export
predictLearner.fdaclassif.knn = function(.learner, .model, .newdata, ...) {

  # transform the data into fda.usc:fdata class type.
  nd = fda.usc::fdata(mdata = .newdata)
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  predict(.model$learner.model, nd, type = type)$prob.group

}
