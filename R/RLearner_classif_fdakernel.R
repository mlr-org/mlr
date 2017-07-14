#' @title Learner for kernel classification for functional data.
#'
#' @description
#' Learner for kernel Classification.
#'
#' @export
makeRLearner.classif.fdakernel = function() {
  makeRLearnerClassif(
    cl = "classif.fdakernel",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "h"),
      makeDiscreteLearnerParam(id = "Ker", default = "AKer.norm", values = list("AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar", "AKer.unif")),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = c("metric.lp", "metric.kl",
        "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(draw = FALSE),
    properties = c("twoclass", "multiclass", "functionals"),
    name = "Kernel classification on FDA",
    short.name = "fdakernel",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.classif.fdakernel = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  data.fdclass = fda.usc::fdata(mdata = z$data)
  glearn = z$target
  learned.model = fda.usc::classif.kernel(group = glearn, fdataobj = data.fdclass,...)
  return(learned.model)
}

#' @export
predictLearner.classif.fdakernel = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd.fdclass = fda.usc::fdata(mdata = .newdata)
  class.pred = fda.usc::predict.classif(object = m, new.fdataobf = nd.fdclass, ...)
  return(class.pred)
}
