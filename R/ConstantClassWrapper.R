#' @title Wraps a classification learner to support problems where the class label is (almost) constant.
#'
#' @description
#' If the training data contains only a single class (or almost only a single class), this wrapper creates a model that always predicts the constant class in the training data. In all other cases, the underlying learner is trained and the resulting model used for predictions.
#'
#' Probabilities can be predicted and will be 1 or 0 depending on whether the label matches the majority class or not.
#'
#' @template arg_learner
#' @param frac `numeric(1)`\cr
#' The fraction of labels in [0, 1) that can be different from the majority label. Default is 0, which means that constant labels are only predicted if there is exactly one label in the data.
#' @template ret_learner
#' @family wrapper
#' @export
makeConstantClassWrapper = function(learner, frac = 0) {
  learner = checkLearner(learner, "classif")

  lrn = makeBaseWrapper(
    id = paste(learner$id, "nearlyconstantclass", sep = "."),
    type = learner$type,
    next.learner = learner,
    package = learner$package,
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "frac", lower = 0, upper = 1, default = 0)
    ),
    par.vals = list(frac = frac),
    learner.subclass = "ConstantClassWrapper",
    model.subclass = "ConstantClassModel")

  return(lrn)
}

#' @export
trainLearner.ConstantClassWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, frac = 0, ...) {
  labels.distribution = sort(prop.table(table(getTaskTargets(subsetTask(.task, .subset)))), decreasing = TRUE)
  most.frequent = labels.distribution[1L]
  if (most.frequent >= (1 - frac)) {
    mod = makeS3Obj("ConstantClassModelConstant",
      label = factor(names(most.frequent)),
      levels = .task$task.desc$class.levels)
    m = makeWrappedModel.Learner(.learner, mod, getTaskDesc(.task), .subset,
      getTaskFeatureNames(.task), getTaskFactorLevels(.task), 0)
  } else {
    m = train(.learner$next.learner, .task, .subset, weights = .weights)
  }
  cm = makeChainModel(next.model = m, cl = "ConstantClassModel")
  return(cm)
}

#' @export
predictLearner.ConstantClassWrapper = function(.learner, .model, .newdata, ...) {
  mod = .model$learner.model$next.model$learner.model
  if (inherits(mod, "ConstantClassModelConstant")) {
    switch(.learner$predict.type,
      response = rep.int(mod$label, nrow(.newdata)),
      prob = matrix(as.numeric(mod$levels == mod$label),
        ncol = length(mod$levels), nrow = nrow(.newdata),
        byrow = TRUE, dimnames = list(NULL, mod$levels))
    )
  } else {
    NextMethod()
  }
}
