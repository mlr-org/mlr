#' @export
makeRLearner.regr.fgam = function() {
  makeRLearnerRegr(
    cl = "regr.fgam",
    package = "refund",
    par.set = fgam.ps,
    par.vals = fgam.par.vals,
    properties = c("functionals", "single.functional"),
    name = "functional general additive model",
    short.name = "FGAM"
  )
}

#' @export
trainLearner.regr.fgam = function(.learner, .task, .subset, .weights = NULL, ...) {
  requirePackages("refund")
  parlist = list(...)
  m = getTaskData(.task, functionals.as = "matrix")
  tn = getTaskTargetNames(.task)
  fns = getTaskFeatureNames(.task)
  formmat = getFGAMFormulaMat(mdata = m, targetname = tn, fns = fns, parlist = parlist)
  pfr = refund::pfr
  pfr(formula = formmat$form, data = formmat$mat.list, family = gaussian())
}

#' @export
predictLearner.regr.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  as.vector(predict(.model$learner.model, newdata = nl, type = "response"))
}
