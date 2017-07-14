#' @title Classification of functional data by Generalized Linear Models.
#'
#' @description
#' Learner for classification using Generalized Linear Models.
#'
#' @export
makeRLearner.classif.fdaglm = function() {
  makeRLearnerClassif(
    cl = "classif.fdaglm",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial()", values = list("binomial()", "gaussian()", "Gamma()", "inverse.gaussian()", "poisson()")),
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "probs", "functionals"),
    name = "Generalized Linear Models classification on FDA",
    short.name = "fdaglm"
  )
}

#' @export
trainLearner.classif.fdaglm = function(.learner, .task, .subset, .weights = NULL, ...) {

  # Get data and transform to functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, keep.functionals = TRUE)
  fd = d$data[, which(lapply(d$data, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = fda.usc::fdata(mdata = setClasses(fd, "matrix")))
  model = fda.usc::classif.glm(d.target ~ x, data = dat)
  # Fix bug in package
  model$C[[1]] = quote(classif.glm)
  return(model)
}

#' @export
predictLearner.classif.fdaglm = function(.learner, .model, .newdata, ...) {
  # transform the data into fda.usc:fdata class type.
  fd = .newdata[, which(lapply(.newdata, function(x) class(x)[1]) %in% c("functional" , "matrix"))]
  if (ncol(fd) == 0)
    stop("No functional features in the data")
  nd = list(x = fda.usc::fdata(mdata = setClasses(fd, "matrix")))

  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)$prob.group
  } else {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)
  }
}
