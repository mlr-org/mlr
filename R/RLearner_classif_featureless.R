#' @export
makeRLearner.classif.featureless = function() {
  makeRLearnerClassif(
    cl = "classif.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeUntypedLearnerParam(
        id = "measure",
        default = mmce,
        tunable = TRUE
      )
    ),
    par.vals = list(measure = mmce),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Featureless classifier",
    short.name = "featurelessClassif"
  )
}


#' @export
trainLearner.classif.featureless = function(.learner,
                                            .task,
                                            .subset,
                                            .weights = NULL,
                                            measure = mmce,
                                            ...) {
  levs = getTaskClassLevels(.task)
  y = getTaskTargets(.task)
  n = length(y)
  scores = vnapply(levs, function(a) {
    arep = factor(rep(a, n), levels = levs)
    data = data.frame(truth = y, response = arep)
    desc = makeS3Obj("TaskDesc", class.levels = levs)
    p = makeS3Obj("Prediction", data = data, task.desc = desc)
    measure$fun(pred = p, extra.args = measure$extra.args)
  })
  factor(names(scores)[which.min(scores)], levels = levs)
}


#' @export
predictLearner.classif.featureless = function(.learner, .model, .newdata, ...) {
  lev = .model$learner.model
}
