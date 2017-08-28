#' @title Learner for kernel classification for functional data.
#'
#' @description
#' Learner for kernel Classification.
#'
#' @export
makeRLearner.fdaclassif.kernel = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.kernel",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "h"),
      makeDiscreteLearnerParam(id = "Ker", default = "AKer.norm", values = list("AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar", "AKer.unif")),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = list("metric.lp", "metric.kl", "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = list("GCV.S", "CV.S", "GCCV.S")),
      makeUntypedLearnerParam(id = "par.CV"),
      makeUntypedLearnerParam(id = "par.S")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Kernel classification on FDA",
    short.name = "kernelFDA"
  )
}

#' @export
trainLearner.fdaclassif.kernel = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  data.fdclass = fda.usc::fdata(mdata = z$data)
  glearn = z$target
  learned.model = fda.usc::classif.kernel(group = glearn, fdataobj = data.fdclass,...)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.kernel = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd = fda.usc::fdata(mdata = .newdata)
  if (type == "probs") {
    predict(.model$learner.model, nd, type = type)$prob.group
  } else {
    predict(.model$learner.model, nd, type = type)
  }
}
