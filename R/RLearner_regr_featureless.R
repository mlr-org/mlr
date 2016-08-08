#' @export
makeRLearner.regr.featureless = function() {
  makeRLearnerRegr(
    cl = "regr.featureless",
    package = "mlr",
    par.set = makeParamSet(addClasses(makeUntypedLearnerParam(id = "measure", 
      default = mse, tunable = TRUE), "MeasureParam")),
    par.vals = list(measure = mse),
    properties = c("numerics"),
    name = "Featureless regressor",
    short.name = "featurelessRegressor"
  )
}

#' @export
trainLearner.regr.featureless = function(.learner, .task, .subset, .weights = NULL, measure = mse, ...) {
  
  if (.task$type %nin% measure$properties)
    stopf("Measure %s does not support task type %s!", measure$id, .task$type)
  if ("req.model" %in% measure$properties)
    stopf("Measure %s requires a fitted model and cannot be used to define a learner!", measure$id)

  
  y = getTaskTargets(.task)
  n = length(y)
  f = function (a) {
    arep = rep(a, n)
    data = data.frame(truth = y, response = arep)
    desc = makeS3Obj("TaskDesc")
    p = makeS3Obj("Prediction", data = data, task.desc = desc)
    measure$fun(pred = p, extra.args = measure$extra.args)
  }
  xmin = optimize(f, c(min(y), max(y)), tol = 0.0001, maximum = !measure$minimize)
  xmin[[1]]
}


#' @export
predictLearner.regr.featureless = function(.learner, .model, .newdata, ...) {
  rep(.model$learner.model, times = nrow(.newdata))
}
