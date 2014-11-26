makeHomogeneousEnsemble = function(id, next.learner, package, par.set = makeParamSet(),
  learner.subclass, model.subclass, ...) {
  x = makeBaseWrapper(id, next.learner, package, par.set,
    learner.subclass = c(learner.subclass, "HomogeneousEnsemble"),
    model.subclass = c(model.subclass, "HomogeneousEnsembleModel"),
    ...)
}

##############################        HomogeneousEnsembleModel            ##############################

#' @export
# if ANY model in the list is broken --> failure
isFailureModel.HomogeneousEnsembleModel = function(model) {
  mods = getHomogeneousEnsembleModels(model, learner.models = FALSE)
  any(vlapply(mods, isFailureModel))
}

#' Returns the list of fitted models.
#'
#' @param model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training a learner of homogeneous models.
#' @param learner.models [\code{logical(1)}]\cr
#'   Return underlying R models or wrapped
#'   mlr models (\code{\link[mlr]{WrappedModel}}).
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
getHomogeneousEnsembleModels = function(model, learner.models = FALSE) {
  assertClass(model, classes = "HomogeneousEnsembleModel")
  ms = model$learner.model$next.model
  if (learner.models)
    extractSubList(ms, "learner.model", simplify = FALSE)
  else
    ms
}

##############################               helpers                      ##############################

# internal mini helper: return a matrix of predictions, either numeric for regr or character for classif
# rows = newdata points, cols = ensembles members
# does only work for responses, not probs, se, etc
predictHomogeneousEnsemble = function(.learner, .model, .newdata, ...) {
  classes = .model$task.desc$class.levels
  models = getHomogeneousEnsembleModels(.model, learner.models = FALSE)
  # for classif we convert factor to char, nicer to handle later on
  preds = lapply(models, function(mod) {
    p = predict(mod, newdata = .newdata, ...)$data$response
    if (is.factor(p))
      p = as.character(p)
    return(p)
  })
  do.call(cbind, preds)
}


# call this at end of trainLearner.CostSensRegrWrapper
# FIXME: potentially remove this when ChainModel is removed
makeHomChainModel = function(learner, models) {
  makeChainModel(next.model = models, cl = c(learner$model.subclass, "HomogeneousEnsembleModel"))
}
