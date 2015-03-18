#' @export
makeRLearner.classif.gam = function() {
  makeRLearnerClassif(
    cl = "classif.gam",
    package = "mgcv",
    par.set = makeParamSet(
      # method, optimizer, select
      makeDiscreteLearnerParam(id = "method", default = "GACV.Cp",
                               values = c("GACV.Cp", "GCV.Cp", "REML", "P-REML", "ML", "P-ML")),
      makeNumericLearnerParam(id = "gamma", default = 1, lower = 0),
      makeNumericLearnerParam(id = "scale", default = 0),
      makeNumericVectorLearnerParam(id = "knots"),
      makeNumericVectorLearnerParam(id = "sp"),
      makeNumericLearnerParam(id = "H"),
      makeNumericLearnerParam(id = "in.out"),
      makeLogicalLearnerParam(id = "drop.unused.levels", default = TRUE),
      makeLogicalLearnerParam(id = "select", default = FALSE)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Generalized Additive Models for Classification",
    short.name = "gam",
    note = "Uses mgcv::gam for Binary classification using GAMs"
  )
}

#' @export
trainLearner.classif.gam = function(.learner, .task, .subset, .weights = NULL, ...) {

  ctrl = learnerArgsToControl(mgcv::gam.control)
  f = getTaskFormula(.task, explicit.features = TRUE)  
  mgcv::gam(f, data = getTaskData(.task, .subset), control = ctrl, family = "binomial", ...)
}

#' @export
predictLearner.classif.gam = function(.learner, .model, .newdata, ...) {
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}