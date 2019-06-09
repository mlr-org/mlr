#' @export
makeRLearner.regr.rsm = function() {
  makeRLearnerRegr(
    cl = "regr.rsm",
    package = "rsm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "modelfun", default = "FO", values = c("FO", "TWI", "SO"))
      # FIXME rsm() seems to have no default as one expression is needed for the formula, if there is a default, par.vals is redundant
    ),
    par.vals = list(modelfun = "FO"),
    properties = "numerics",
    name = "Response Surface Regression",
    short.name = "rsm",
    note = 'You select the order of the regression by using `modelfun = "FO"` (first order), `"TWI"` (two-way interactions, this is with 1st oder terms!) and `"SO"` (full second order).',
    callees = "rsm"
  )
}

#' @export
trainLearner.regr.rsm = function(.learner, .task, .subset, .weights = NULL, ...) {
  mf = list(...)$modelfun
  vs = stri_paste(getTaskFeatureNames(.task), collapse = ",", sep = " ")
  g = function(x) stri_paste(x, "(", vs, ")", sep = "")
  mf = switch(mf,
    FO = g("FO"),
    TWI = stri_paste(g("TWI"), "+", g("FO"), sep = " "),
    SO = g("SO"),
    stop("Unknown modelfun: ", mf)
  )
  f = as.formula(stri_paste(getTaskTargetNames(.task), "~", mf, sep = " "))
  myargs = list(f, getTaskData(.task, .subset))
  # strange behaviour in rsm forces us to use do.call...
  do.call(rsm::rsm, myargs)
}

#' @export
predictLearner.regr.rsm = function(.learner, .model, .newdata, ...) {
  as.numeric(predict(.model$learner.model, newdata = .newdata, ...))
}
