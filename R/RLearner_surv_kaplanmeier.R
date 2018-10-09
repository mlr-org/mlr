#' @export
makeRLearner.surv.kaplanmeier = function() {
  makeRLearnerSurv(
    cl = "surv.kaplanmeier",
    package = "survival",
    par.set = makeParamSet(),
    properties = c("numerics", "factors", "weights", "prob"),
    name = "Kaplan-Meier reference estimate for survival analysis",
    short.name = "surv.km",
    note = "",
    callees = c("survival")
  )
}

#' @export
trainLearner.surv.kaplanmeier = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset)
  target.names = getTaskTargetNames(.task)
  mod = survfit(Surv(time, status) ~ 1, data = data[, target.names])
  mod = mlr:::attachTrainingTime(mod, .task, data)
  mod
}

#' @export
predictLearner.surv.kaplanmeier = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    rep(median(.model$learner.model$times), nrow(.newdata))
  } else {
    # fill something arbitrary for preds
    preds = rep(median(.model$learner.model$times), nrow(.newdata))
    train.times = sort(unique(c(0, .model$learner.model$times)))
    survest = stepfun(.model$learner.model$times, c(1, .model$learner.model$surv))
    prob = survest(train.times)
    probs = matrix(rep(prob, nrow(.newdata)), byrow = T, nrow = nrow(.newdata))
    list(preds = preds, probs = probs, train.times = train.times)
  }
}
