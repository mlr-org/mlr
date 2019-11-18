#' @export
makeRLearner.classif.fgam = function() {
  makeRLearnerClassif(
    cl = "classif.fgam",
    package = "refund",
    par.set = fgam.ps,
    par.vals = fgam.par.vals,
    properties = c("functionals", "single.functional", "twoclass", "prob"),
    name = "functional general additive model",
    short.name = "FGAM"
  )
}

#' @export
trainLearner.classif.fgam = function(.learner, .task, .subset, .weights = NULL, ...) {

  parlist = list(...)
  tn = getTaskTargetNames(.task)
  dd = getTaskData(.task, target.extra = FALSE, functionals.as = "matrix")

  # tranform target to 0, 1
  dd[[tn]] = as.integer(dd[[tn]]) - 1
  formmat = getFGAMFormulaMat(mdata = dd, targetname = tn, fns = getTaskFeatureNames(.task), parlist)
  pfr = refund::pfr # Weird hack but required since refund behaves weird when it is not loaded.
  mod = pfr(formula = formmat$form, data = formmat$mat.list, family = binomial())
  mod$uvt = unique(getTaskTargets(.task))
  return(mod)
}

#' @export
predictLearner.classif.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)

  pred = predict(.model$learner.model, newdata = nl, type = "response")
  if (.learner$predict.type == "prob") {
    return(propVectorToMatrix(pred, c(.model$task.desc$negative, .model$task.desc$positive)))
  } else {
    return(factor(round(pred), labels = .model$learner.model$uvt))
  }
}
