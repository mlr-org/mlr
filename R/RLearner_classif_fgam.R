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
  requirePackages("refund")

  parlist = list(...)
  tn = getTaskTargetNames(.task)
  fns = getTaskFeatureNames(.task)
  # tranform target to be 0 1
  vt = getTaskTargets(.task)
  uvt = unique(vt)
  dd = getTaskData(.task, target.extra = TRUE, functionals.as = "matrix")
  newtarget = sapply(dd$target, function(x) {
    if (x == uvt[1]) return(1); return(0)})
  nd = cbind(dd$data, newtarget)
  colnames(nd)[ncol(nd)] = tn
  formmat = getFGAMFormulaMat(mdata = nd, targetname = tn, fns = fns, parlist)
  formula = formmat$form
  data = formmat$mat.list
  pfr = refund::pfr
  mod = pfr(formula = formula, data = data, family = binomial())
  mod$uvt = uvt
  mod
}

#' @export
predictLearner.classif.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  pred = predict(.model$learner.model, newdata = nl, type = "response")  # predict.fgam, predict.gam, predict.pfr
  if (.learner$predict.type  == "prob") {
    return(as.vector(pred))
  } else {
  uvt = .model$learner.model$uvt
  newpred = round(pred)
  newpred = sapply(newpred, function(x) {
    if (x == 1) return(uvt[1]); return(uvt[2])})
  newpred = as.factor(newpred)
  return(newpred)
  }
}

