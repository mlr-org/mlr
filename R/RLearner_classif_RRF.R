#' @export
makeRLearner.classif.RRF = function() {
  makeRLearnerClassif(
    cl = "classif.RRF",
    package = "RRF",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented:
      # mtry = floor(ncol(x)/3)
      makeIntegerLearnerParam(id = "mtry", lower = 1L, default = 5L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "flagReg", default = 1L, lower = 0),
      makeNumericLearnerParam(id = "coefReg", default = 0.8,
        requires = quote(flagReg == 1L)),
      makeIntegerVectorLearnerParam(id = "feaIni", lower = 0, upper = Inf,
        requires = quote(flagReg == 1L)),
      makeNumericVectorLearnerParam(id = "classwt", lower = 0, upper = 1L),
      makeNumericVectorLearnerParam(id = "cutoff", lower = 0, upper = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeIntegerLearnerParam(id = "nPerm", lower = 1L, default = 1L, tunable = FALSE),
      makeLogicalLearnerParam(id = "proximity", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "oob.prox", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "norm.votes", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE),
      makeUntypedLearnerParam(id = "strata"),
      makeIntegerVectorLearnerParam(id = "sampsize", lower = 0)
    ),
    properties = c("twoclass", "multiclass", "prob", "numerics", "factors", "featimp"),
    name = "Regularized Random Forests",
    short.name = "RRF",
    note = "",
    callees = "RRF"
  )
}

#' @export
trainLearner.classif.RRF = function(.learner, .task, .subset, .weights, ...) {
  RRF::RRF(formula = getTaskFormula(.task), data = getTaskData(.task, .subset),
    keep.forest = TRUE, ...)
}

#' @export
predictLearner.classif.RRF = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  p = predict(object = .model$learner.model, newdata = .newdata, type = type, ...)
  return(p)
}

#' @export
getFeatureImportanceLearner.classif.RRF = function(.learner, .model, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  ctrl = list(...)
  if (is.null(ctrl$type)) {
    ctrl$type = 2L
  } else if (ctrl$type == 1L) {
    has.fiv = .learner$par.vals$importance
    if (is.null(has.fiv) || has.fiv != TRUE) {
      stop("You need to train the learner with parameter 'importance' set to TRUE")
    }
  }

  RRF::importance(mod, ctrl$type)[, 1]
}
