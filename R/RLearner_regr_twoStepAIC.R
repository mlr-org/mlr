selectForward2Step = function(d, target, muffle = FALSE) {
  feats = setdiff(colnames(d), target)
  f1 = reformulate(termlabels = "1", response = target)
  print(f1)
  f2 = sprintf("%s ~ %s", target, collapse(feats, "+"))
  print(f2)
  f2 = as.formula(f2)
  m0 = lm(f1, data = d)
  n = nrow(d)
  g = if (muffle) capture.output else identity
  st = system.time({
    g({
      m1 = MASS::stepAIC(m0, direction = "forward", scope = list(lower = f1, upper = f2), trace = -1, k = log(n))
    })
  })
  runtime = st[3L]
  cc1 = coef(m1)
  #print(cc1)
  messagef("size of model: %i; runtime[s]: %g", length(cc1) - 1L, runtime)
  #save2(model1 = m1, runtime = runtime, file = "steplm1.RData")
  f3 = sprintf("%s ~ (%s)^2 - 1", target, collapse(names(cc1)[-1], "+"))
  #print(f3)
  system.time({
    g({
      m2 = MASS::stepAIC(m0, direction = "forward", scope = list(lower = f1, upper = f3), trace = -1, k = log(n))
    })
  })
  cc2 = coef(m2)
  #print(cc2)
  runtime = st[3L]
  messagef("size of model: %i; runtime[s]: %g", length(cc2) - 1L, runtime)
  #save2(model2 = m2, runtime = runtime, file = "steplm2.RData")
  return(m2)
}

#' @export
makeRLearner.regr.twoStepAIC = function() {
  makeRLearnerRegr(
    cl = "regr.twoStepAIC",
    package = "MASS",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "muffle", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors"),
    name = "Simple two Step AIC for linear regression",
    short.name = "tsAIC"
  )
}

#' @export
trainLearner.regr.twoStepAIC = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  
  mmodel = selectForward2Step(d = d, target = getTaskTargetNames(.task), muffle = FALSE)
  return(mmodel)
  #stats::lm(f, data = d, weights = .weights, ...)
}

#' @export
predictLearner.regr.twoStepAIC = function(.learner, .model, .newdata, ...) {
    predict(.model$learner.model, newdata = .newdata, ...)
}
