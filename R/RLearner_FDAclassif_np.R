#' @title Learner for nonparametric classification for functional data.
#'
#' @description
#' Learner for Nonparametric Supervised Classification.
#'
#' @export
makeRLearner.fdaclassif.np = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.np",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "h"),
      makeDiscreteLearnerParam(id = "Ker", default = "AKer.norm", values = list("AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar", "AKer.unif")),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = list("metric.lp", "metric.kl", "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = list("GCV.S", "CV.S", "GCCV.S")),
      makeUntypedLearnerParam(id = "par.CV"),
      makeDiscreteLearnerParam(id = "type.S", default = "S.NW", values = list("S.NW", "S.LLR", "S.KNN")),
      makeUntypedLearnerParam(id = "par.S")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Nonparametric classification on FDA",
    short.name = "npFDA"
  )
}

#' @export
trainLearner.fdaclassif.np = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  data.fdclass = fda.usc::fdata(mdata = z$data)
  glearn = z$target
  learned.model = fda.usc::classif.np(group = glearn, fdataobj = data.fdclass,...)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.np = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd = fda.usc::fdata(mdata = .newdata)
  m$C[[1]] = quote(classif.np)
  type = ifelse(.learner$predict.type == "prob","prob", "class")
  if (type == "probs") {
    predict(.model$learner.model, nd, type = type)$prob.group
  } else {
    predict(.model$learner.model, nd, type = type)
  }
}
