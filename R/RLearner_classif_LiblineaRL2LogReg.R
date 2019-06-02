#' @export
makeRLearner.classif.LiblineaRL2LogReg = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRL2LogReg",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = 0L, values = c(0L, 7L)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # FIXME: Add default value when parameter dependent defaults are implemented:
      ## if type = 0: eps default = 0.01, if type = 7: eps default = 0.1
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(type = 0L),
    # FIXME default in LiblieaR() for type is 0, par.vals is redundant here.
    properties = c("twoclass", "multiclass", "numerics", "class.weights", "prob"),
    class.weights.param = "wi",
    name = "L2-Regularized Logistic Regression",
    short.name = "liblinl2logreg",
    note = "`type = 0` (the default) is primal and `type = 7` is dual problem.",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.classif.LiblineaRL2LogReg = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, ...)
}

#' @export
predictLearner.classif.LiblineaRL2LogReg = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
  } else {
    predict(.model$learner.model, newx = .newdata, proba = TRUE, ...)$probabilities
  }
}
