#' @title Learner for kernel classification for functional data.
#'
#' @description
#' Learner for kernel Classification.
#'
#' @export
makeRLearner.classif.fdausc.kernel = function() {
  makeRLearnerClassif(
    cl = "classif.fdausc.kernel",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "h", default = NULL, special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "Ker", default = "AKer.norm",
        values = list("AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar", "AKer.unif")),
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp", values = c("metric.lp", "metric.kl",
        "metric.hausdorff", "metric.dist")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S", values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(draw = FALSE),
    properties = c("twoclass", "multiclass", "prob", "single.functional"),
    name = "Kernel classification on FDA",
    short.name = "fdausc.kernel",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.classif.fdausc.kernel = function(.learner, .task, .subset, .weights = NULL, trim, draw, metric, Ker, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)

  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  par.cv = learnerArgsToControl(list, trim, draw)
  par.funs = learnerArgsToControl(list, metric, Ker)
  par.funs = lapply(par.funs, function(x) getFromNamespace(x, "fda.usc"))

  trainfun = getFromNamespace("classif.kernel", "fda.usc")
  mod = do.call("trainfun",
    c(list(group = d$target, fdataobj = data.fdclass, par.CV = par.cv, par.S = list(w = .weights)),
      list(metric = par.funs$metric)[which(names(par.funs) == "metric")],
      list(Ker = par.funs$Ker)[which(names(par.funs) == "Ker")],
      ...))
}

#' @export
predictLearner.classif.fdausc.kernel = function(.learner, .model, .newdata, ...) {
  # transform the data into fda.usc:fdata class type.
  fd = getFunctionalFeatures(.newdata)
  nd = fda.usc::fdata(mdata = as.matrix(fd))

  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    predict(.model$learner.model, nd, type = type)$prob.group
  } else {
    predict(.model$learner.model, nd, type = type)
  }
}
