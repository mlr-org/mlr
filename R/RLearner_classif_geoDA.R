#' @export
makeRLearner.classif.geoDA = function() {
  makeRLearnerClassif(
    cl = "classif.geoDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "validation", values = list(crossval = "crossval", learntest = "learntest", NULL = NULL), default = NULL, tunable = FALSE)
    ),
    par.vals = list(validation = NULL),
    # FIXME default of geoDa for validation is NULL, par.vals is redundant here.
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Geometric Predictive Discriminant Analysis",
    short.name = "geoda",
    callees = c("geoDA", "classify")
  )
}

#' @export
trainLearner.classif.geoDA = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "drop.levels")
  DiscriMiner::geoDA(variables = d$data, group = d$target, ...)
}

#' @export
predictLearner.classif.geoDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = DiscriMiner::classify(m, newdata = .newdata)
  # p$scores #we loose this information
  p$pred_class
}
