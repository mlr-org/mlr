#' @export
makeRLearner.regr.RRF = function() {
  makeRLearnerRegr(
    cl = "regr.RRF",
    package = "RRF",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars)) 
      makeIntegerLearnerParam(id = "mtry", lower = 1L), 
      # FIXME: Add default value when data dependent defaults are implemented: min.node.size = 1 for classification,
      # 10 for probability prediction 
      # FIXME: add args strata and sampsize and corr.bias, feaIni
      makeIntegerLearnerParam(id = "nodesize", lower = 5L), 
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "flagReg", default = 1L, lower = 0), 
      makeNumericLearnerParam(id = "coefReg", default = 0.8, requires = expression(flagReg == 1L)),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeIntegerLearnerParam(id = "nPerm", lower = 1L, default = 1L, tunable = FALSE),
      makeLogicalLearnerParam(id = "proximity", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "oob.prox", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "ordered"),
    name = "Regularized Random Forests",
    short.name = "RRF",
    note = ""
  )
}

#' @export
trainLearner.regr.RRF <- function(.learner, .task, .subset, .weights, ...) {
  args = list(...)
  RRF::RRF(formula = getTaskFormula(.task), data = getTaskData(.task, .subset), 
           keep.forest= TRUE, ...)
}

#' @export
predictLearner.regr.RRF <- function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, newdata = .newdata, ...)
  return(p)
}
