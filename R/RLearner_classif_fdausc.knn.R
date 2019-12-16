#' @export
makeRLearner.classif.fdausc.knn = function() {
  makeRLearnerClassif(
    cl = "classif.fdausc.knn",
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
    properties = c("twoclass", "multiclass", "weights", "prob", "single.functional"),
    name = "fdausc.knn",
    short.name = "fdausc.knn",
    note = "Argument draw=FALSE is used as default."
  )
}

#' @export
trainLearner.classif.fdausc.knn = function(.learner, .task, .subset, .weights = NULL, trim, draw, metric, ...) {


  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)

  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  par.cv = learnerArgsToControl(list, trim, draw)

  par.funs = learnerArgsToControl(list, metric)
  par.funs = lapply(par.funs, function(x) getFromNamespace(x, "fda.usc"))

  trainfun = getFromNamespace("classif.knn", "fda.usc")
  # supress printer
  mod = suppressAll(do.call("trainfun",
    c(list(group = d$target, fdataobj = data.fdclass, par.CV = par.cv, par.S = list(w = .weights)),
      list(metric = par.funs$metric)[which(names(par.funs) == "metric")],
      ...)))
}

#' @export
predictLearner.classif.fdausc.knn = function(.learner, .model, .newdata, ...) {
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
