
makeRLearner.regr.fregre.glm = function() {
  makeRLearnerRegr(
    cl = "regr.fregre.glm",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial()", values = list("binomial()", "gaussian()", "Gamma()", "inverse.gaussian()", "poisson()")),
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE)
    ),
    properties = c("functionals"),
    name = "Generalized Linear Models regression on FDA",
    short.name = "fregre.glm",
    note = "model$C[[1]] is set to quote(fregre.glm)"
  )
}


trainLearner.regr.fregre.glm = function(.learner, .task, .subset, .weights = NULL, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = data.fdclass)

  model = fda.usc::fregre.glm(d.target ~ x, data = dat)

  # Fix bug in package. The changed slot looks different when called with
  # `fda.usc::lassif.glm()` than just `classif.glm()`
  model$call[[1]] = quote(fregre.glm)

  return(model)
}


predictLearner.regr.fregre.glm = function(.learner, .model, .newdata, ...) {
  # transform the data into fda.usc:fdata class type.
  fd = getFunctionalFeatures(.newdata)
  nd = list(x = fda.usc::fdata(mdata = fd))

  predict(object = .model$learner.model, newx = nd, type = "response")

}



registerS3method("makeRLearner", "regr.fregre.glm",
                 makeRLearner.regr.fregre.glm)
registerS3method("trainLearner", "regr.fregre.glm",
                 trainLearner.regr.fregre.glm)
registerS3method("predictLearner", "regr.fregre.glm",
                 predictLearner.regr.fregre.glm)

parallelExport("trainLearner.regr.fregre.glm",
               "predictLearner.regr.fregre.glm")
