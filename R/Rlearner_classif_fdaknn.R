#' @export
makeRLearner.classif.fdaknn = function() {
  makeRLearnerClassif(
    cl = "classif.fdaknn",
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
    properties = c("twoclass", "multiclass", "numerics", "weights", "prob", "functionals"),
    name = "fdaknn",
    short.name = "fdaknn",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.classif.fdaknn = function(.learner, .task, .subset, .weights = NULL, trim, draw, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, keep.functionals = TRUE)
  fd = d$data[, which(lapply(d$data, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  # transform the data into fda.usc:fdata class type.

  data.fdclass = fda.usc::fdata(mdata = setClasses(fd, "matrix"))
  par.cv = learnerArgsToControl(list, trim, draw)
  fda.usc::classif.knn(group = d$target, fdataobj = data.fdclass, par.CV = par.cv,
    par.S = list(w = .weights), ...)
 }

#' @export
predictLearner.classif.fdaknn = function(.learner, .model, .newdata, ...) {


  # transform the data into fda.usc:fdata class type.
  fd = .newdata[, which(lapply(.newdata, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  nd = fda.usc::fdata(mdata = setClasses(fd, "matrix"))

  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    predict(.model$learner.model, nd, type = type)$prob.group
  } else {
    predict(.model$learner.model, nd, type = type)
  }
}
