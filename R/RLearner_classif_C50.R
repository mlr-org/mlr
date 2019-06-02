#' @export
makeRLearner.classif.C50 = function() {
  makeRLearnerClassif(
    cl = "classif.C50",
    package = "C50",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "trials", lower = 1L, default = 1L),
      makeLogicalLearnerParam(id = "rules", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "subset", default = FALSE),
      # FIXME: Default = 0 throws error because 'lower' = 2L  is above default.
      makeIntegerLearnerParam(id = "bands", lower = 2L, upper = 1000L,
        tunable = FALSE, requires = quote(rules == TRUE)),
      makeLogicalLearnerParam(id = "winnow", default = FALSE),
      makeLogicalLearnerParam(id = "noGlobalPruning", default = FALSE),
      makeNumericLearnerParam(id = "CF", lower = 0, upper = 1, default = 0.25),
      # FIXME: upper limit is data dependent
      makeIntegerLearnerParam(id = "minCases", lower = 0L, upper = Inf, default = 2L),
      makeLogicalLearnerParam(id = "fuzzyThreshold", default = FALSE),
      makeNumericLearnerParam(id = "sample", lower = 0, upper = .999, default = 0, tunable = TRUE),
      makeIntegerLearnerParam(id = "seed", lower = -Inf, upper = Inf, tunable = FALSE),
      makeLogicalLearnerParam(id = "earlyStopping", default = TRUE),
      # label just changes the word 'outcome' to something else in the output
      makeUntypedLearnerParam(id = "label", default = "outcome", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "missings", "weights"),
    name = "C50",
    short.name = "C50",
    callees = c("C5.0", "C5.0Control")
  )
}

#' @export
trainLearner.classif.C50 = function(.learner, .task, .subset, .weights = NULL,
  subset, bands, winnow, noGlobalPruning, CF, minCases, fuzzyThreshold, sample,
  seed, earlyStopping, label, ...) {
  ctrl = learnerArgsToControl(C50::C5.0Control, bands, winnow, subset,
    noGlobalPruning, CF, minCases, fuzzyThreshold, sample, seed, earlyStopping,
    label)

  d = getTaskData(.task, .subset, target.extra = TRUE)
  C50::C5.0(x = d$data, y = d$target, control = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.classif.C50 = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  pred.type = .learner$predict.type
  if (pred.type == "response") {
    pred.type = "class"
  }
  preds = predict(m, newdata = .newdata, type = pred.type, ...)
  return(preds)
}
