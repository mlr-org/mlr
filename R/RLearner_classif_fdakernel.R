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
      makeIntegerVectorLearnerParam(id = "h", default = NULL, special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "Ker", default = "AKer.norm", values = list("AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar", "AKer.unif")),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = c("metric.lp", "metric.kl",
        "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(draw = FALSE),
    properties = c("twoclass", "multiclass", "probs", "functionals"),
    name = "Kernel classification on FDA",
    short.name = "fdakernel",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.classif.fdakernel = function(.learner, .task, .subset, .weights = NULL, trim, draw, ...) {
  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, keep.functionals = TRUE)
  fd = d$data[, which(lapply(d$data, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  # transform the data into fda.usc:fdata class type.

  data.fdclass = fda.usc::fdata(mdata = setClasses(fd, "matrix"))
  par.cv = learnerArgsToControl(list, trim, draw)
  fda.usc::classif.kernel(group = d$target, fdataobj = data.fdclass, par.CV = par.cv,
    par.S = list(w = .weights), ...)
}

#' @export
predictLearner.classif.fdakernel = function(.learner, .model, .newdata, ...) {

  # transform the data into fda.usc:fdata class type.
  fd = .newdata[, which(lapply(.newdata, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  if (ncol(fd) == 0)
    stop("No functional features in the data")
  nd = fda.usc::fdata(mdata = setClasses(fd, "matrix"))

  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    predict(.model$learner.model, nd, type = type)$prob.group
  } else {
    predict(.model$learner.model, nd, type = type)
  }
}
