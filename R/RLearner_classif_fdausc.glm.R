#' @title Classification of functional data by Generalized Linear Models.
#'
#' @description
#' Learner for classification using Generalized Linear Models.
#'
#' @export
makeRLearner.classif.fdausc.glm = function() {
  makeRLearnerClassif(
    cl = "classif.fdausc.glm",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial()", values = list("binomial()", "gaussian()", "Gamma()", "inverse.gaussian()", "poisson()")),
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "prob", "functionals"),
    name = "Generalized Linear Models classification on FDA",
    short.name = "fdausc.glm",
    note = "model$C[[1]] is set to quote(classif.glm)"
  )
}

#' @export
trainLearner.classif.fdausc.glm = function(.learner, .task, .subset, .weights = NULL, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = data.fdclass)

  model = fda.usc::classif.glm(d.target ~ x, data = dat)

  # Fix bug in package. The changed slot looks different when called with
  # `fda.usc::lassif.glm()` than just `classif.glm()`
  model$C[[1]] = quote(classif.glm)

  return(model)
}

#' @export
predictLearner.classif.fdausc.glm = function(.learner, .model, .newdata, ...) {
  # transform the data into fda.usc:fdata class type.
  fd = getFunctionalFeatures(.newdata)
  nd = list(x = fda.usc::fdata(mdata = fd))
  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    predict(object = .model$learner.model, new.fdataobj = nd, type = type)$prob.group
  } else {
    predict(object = .model$learner.model, new.fdataobj = nd, type = type)
  }
}
