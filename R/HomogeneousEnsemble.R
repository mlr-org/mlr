makeHomogeneousEnsemble = function(id, type, next.learner, package, par.set = makeParamSet(),
  learner.subclass, model.subclass, ...) {
  makeBaseWrapper(id, type, next.learner, package, par.set,
    learner.subclass = c(learner.subclass, "HomogeneousEnsemble"),
    model.subclass = c(model.subclass, "HomogeneousEnsembleModel"),
    ...)
}

##############################        HomogeneousEnsembleModel            ##############################

#' @export
# if ANY model in the list is broken --> failure
isFailureModel.HomogeneousEnsembleModel = function(model) {
  mods = getLearnerModel(model, more.unwrap = FALSE)
  any(vlapply(mods, isFailureModel))
}

#' @export
getFailureModelMsg.HomogeneousEnsembleModel = function(model) {
  mods = getLearnerModel(model, more.unwrap = FALSE)
  msgs = vcapply(mods, getFailureModelMsg)
  j = which.first(!is.na(msgs))
  ifelse(j == 0L, NA_character_, msgs[j])
}

#' @export
getFailureModelDump.HomogeneousEnsembleModel = function(model) {
  mods = getLearnerModel(model, more.unwrap = FALSE)
  msgs = lapply(mods, getFailureModelDump)
  j = which.first(!is.null(msgs))
  ifelse(j == 0L, NULL, msgs[[j]])
}

#' Deprecated, use `getLearnerModel` instead.
#' @param model Deprecated.
#' @param learner.models Deprecated.
#' @export
getHomogeneousEnsembleModels = function(model, learner.models = FALSE) {
  .Deprecated("getLearnerModel")
  getLearnerModel(model, more.unwrap = learner.models)
}

#' @export
getLearnerModel.HomogeneousEnsembleModel = function(model, more.unwrap = FALSE) {
  ms = model$learner.model$next.model
  if (more.unwrap) {
    extractSubList(ms, "learner.model", simplify = FALSE)
  } else {
    ms
  }
}

##############################               helpers                      ##############################

# internal mini helper: return a matrix of predictions, either numeric for regr or character for classif
# rows = newdata points, cols = ensembles members
# does only work for responses, not probs, se, etc
predictHomogeneousEnsemble = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  # for classif we convert factor to char, nicer to handle later on
  preds = lapply(models, function(mod) {
    p = predict(mod, newdata = .newdata, subset = .subset, ...)$data$response
    if (is.factor(p)) {
      p = as.character(p)
    }
    return(p)
  })
  do.call(cbind, preds)
}

# call this at end of trainLearner.CostSensRegrWrapper
# FIXME: potentially remove this when ChainModel is removed
makeHomChainModel = function(learner, models) {
  makeChainModel(next.model = models, cl = c(learner$model.subclass, "HomogeneousEnsembleModel"))
}
