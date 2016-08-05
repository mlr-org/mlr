#' @export
makeRLearner.regr.featureless = function() {
  makeRLearnerRegr(
    cl = "regr.featureless",
    package = "mlr",
    par.set = makeParamSet(
      makeUntypedLearnerParam(
        id = "measure",
        default = mae,
        tunable = TRUE
      )
    ),
    par.vals = list(measure = mae),
    properties = c("numerics"),
    name = "Featureless regressor",
    short.name = "featurelessRegressor"
  )
}

#' @export
trainLearner.regr.featureless = function(.learner,
                                            .task,
                                            .subset,
                                            .weights = NULL,
                                            measure = mmce,
                                            ...) {
  
    y = getTaskTargets(.task)
    n = length(y)
    f <- function (a) {
      arep = rep(a, n)
      data = data.frame(truth = y, response = arep)
      desc = makeS3Obj("TaskDesc")
      p = makeS3Obj("Prediction", data = data, task.desc = desc)
      measure$fun(pred = p, extra.args = measure$extra.args)
    }
    xmin <- optimize(f, c(min(y), max(y)), tol = 0.0001)
    xmin$minimum
}


#' @export
predictLearner.regr.featureless = function(.learner, .model, .newdata, ...) {
  lev = .model$learner.model
}

