#' @title Classification of functional data by Generalized Linear Models.
#'
#' @description
#' Learner for classification using Generalized Linear Models.
#'
#' @export
makeRLearner.fdaclassif.glm = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.glm",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial()", values = list("binomial()", "gaussian()", "Gamma()", "inverse.gaussian()", "poisson()")),
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Generalized Linear Models classification on FDA",
    short.name = "glmFDA"
  )
}

#' @export
trainLearner.fdaclassif.glm = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE)
  data.fdclass = fda.usc::fdata(mdata = z$data)
  dat = list("df" = data.frame(z$target), "x" = data.fdclass)
  learned.model = fda.usc::classif.glm(z.target ~ x, data = dat)
  return(learned.model)
}

#' @export
predictLearner.fdaclassif.glm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  # restructure internal function call (language-object)
  m$C[[1]] = quote(classif.glm)
  # create formulate structure in data
  nd = fda.usc::fdata(mdata = .newdata)
  #nd = list(x = nd.fdclass)
  type = ifelse(.learner$predict.type == "prob", "prob", "class")
  if (type == "probs") {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)$prob.group
  } else {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)
  }
}
