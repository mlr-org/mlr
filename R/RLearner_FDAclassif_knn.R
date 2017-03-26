#' @export
makeRLearner.fdaclassif.knn = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.knn",
    package = "fda.usc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn", lower = 1L, default = NULL,
                              special.vals = list(NULL)),
      # metric.kl throws (cryptic) warning messages
      # metric.dist does (intentionally) not work in the original fda.usc::classif.knn()
      makeDiscreteLearnerParam(id = "metric", default = "metric.lp",
                               values = c("metric.lp",
                                          # "metric.kl", "metric.dist",
                                          "metric.hausdorff",
                                          "inprod.fdata",
                                          "semimetric.basis",
                                          "semimetric.deriv",
                                          "semimetric.fourier",
                                          "semimetric.hshift",
                                          "semimetric.mplsr",
                                          "semimetric.pca")),
      makeDiscreteLearnerParam(id = "type.CV", default = "GCV.S",
                               values = c("GCV.S", "CV.S", "GCCV.S")),
      # trim and draw (= plot!) are the par.CV parameters
      makeNumericLearnerParam(id = "trim", lower = 0L, upper = 1L, default = 0L),
      makeLogicalLearnerParam(id = "draw", default = FALSE, tunable = FALSE),
      # parameters for (semi)metrics
      # TODO parameters type.basis1/2, nbasis1/2 für semimetric.basis implementieren
      # makeDiscreteLearnerParam(id = "type.basis1", default = NULL,
      #                          values = c("bspline", "constant", "exponential",
      #                                     "fourier", "monomial", "pc", "pls",
      #                                     "polygonial", "power"),
      #                          special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "lp", default = 2L,
                              requires = quote(metric == "metric.lp")),
      makeIntegerLearnerParam(id = "nderiv", default = 0L, lower = 0L,
                              requires = quote(metric %in% c("semimetric.basis", "semimetric.deriv",
                                                             "semimetric.fourier"))),
      makeIntegerLearnerParam(id = "nknot", lower = 1L,
                              requires = quote(metric == "semimetric.deriv")),
      makeIntegerLearnerParam(id = "nbasis", lower = 1L,
                              requires = quote(metric == "semimetric.fourier")),
      makeIntegerLearnerParam(id = "q", lower = 1L,
                              default = quote(ifelse(metric == "metric.pca", 1L, 2L)),
                              special.vals = list(quote(ifelse(metric == "metric.pca", 1L, 2L))),
                              requires = quote(metric %in% c("semimetric.pca", "semimetric.mplsr"))),
      makeNumericLearnerParam(id = "period", lower = 0,
                              requires = quote(metric == "semimetric.fourier")),
      # TM: I do not know what this parameter does
      makeUntypedLearnerParam(id = "class1",
                              requires = quote(metric == "metric.mplsr"),
                              tunable = FALSE),
      makeIntegerVectorLearnerParam(id = "t",
                                    lower = 1L, upper = Inf,
                                    requires = quote(metric == "metric.hshift"))

    ),
    par.vals = list(draw = FALSE, metric = "metric.lp"),
    properties = c("twoclass", "multiclass", "numerics", "weights", "prob"),
    name = "Knn on FDA",
    short.name = "knnFDA",
    note = "Draw parameter is set to FALSE as default."
  )
}


#' @export
trainLearner.fdaclassif.knn = function(.learner, .task, .subset, .weights = NULL, trim, draw, metric, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = z$data)
  par.cv = learnerArgsToControl(list, trim, draw)
  par.s = list(w = .weights)
  glearn = z$target
  metric.fun = switch(metric,
                      metric.lp = fda.usc::metric.lp,
                      metric.hausdorff = fda.usc::metric.hausdorff,
                      inprod.fdata = fda.usc::inprod.fdata,
                      semimetric.basis = fda.usc::semimetric.basis,
                      semimetric.deriv = fda.usc::semimetric.deriv,
                      semimetric.fourier = fda.usc::semimetric.fourier,
                      semimetric.hshift = fda.usc::semimetric.hshift,
                      semimetric.mplsr = fda.usc::semimetric.mplsr,
                      semimetric.pca = fda.usc::semimetric.pca
  )
  learned.model = fda.usc::classif.knn(group = glearn, fdataobj = data.fdclass,
                                       par.CV = par.cv, par.S = par.s,
                                       metric = metric.fun, ...)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.knn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd.fdclass = fda.usc::fdata(mdata = .newdata)# transform the data into fda.usc:fdata class type.

  if (.learner$predict.type == "response") {
    return(predict(m, nd.fdclass, type = "class", ...))
  } else if(.learner$predict.type == "prob") {
    pred = predict(m, nd.fdclass, type = "probs", ...)
    return(pred$prob.group)
  }
}
