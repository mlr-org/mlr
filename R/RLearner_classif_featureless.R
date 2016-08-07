#' @export
makeRLearner.classif.featureless = function() {
  makeRLearnerClassif(
    cl = "classif.featureless",
    package = "mlr",
    par.set = makeParamSet(addClasses(makeUntypedLearnerParam(id = "measure", default = mmce, tunable = TRUE), 
      "MeasureParam")),
    par.vals = list(measure = mmce),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Featureless classifier",
    short.name = "featurelessClassif"
  )
}


#' @export
trainLearner.classif.featureless = function(.learner, .task, .subset, .weights = NULL, measure = mmce,...) {
  
  if (.task$type %nin% measure$properties)
    stopf("Measure %s does not support task type %s!", measure$id, .task$type)
  if ("req.model" %in% measure$properties)
    stopf("Measure %s requires a fitted model and cannot be used to define a learner!", measure$id)
  
  levs = getTaskClassLevels(.task)
  
  if (length(levs) > 2L && "classif.multi" %nin% measure$properties)
    stopf("Multiclass problems cannot be used for measure %s!", measure$id)
  
  y = getTaskTargets(.task)
  n = length(y)
  scores = vnapply(levs, function(a) {
    arep = factor(rep(a, n), levels = levs)
    data = data.frame(truth = y, response = arep)
    desc = makeS3Obj("TaskDesc", class.levels = levs)
    p = makeS3Obj("Prediction", data = data, task.desc = desc)
    measure$fun(pred = p, extra.args = measure$extra.args)
  })
  if (measure$minimize)
    factor(names(scores)[which.min(scores)], levels = levs)
  else
    factor(names(scores)[which.max(scores)], levels = levs)
}


#' @export
predictLearner.classif.featureless = function(.learner, .model, .newdata, ...) {
  rep(.model$learner.model, times = nrow(.newdata))
}
