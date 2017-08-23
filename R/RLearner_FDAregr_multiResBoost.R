#' @export
makeRLearner.fdaregr.multiResBoost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.multiResBoost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "max_iter", default = 10L, tunable = TRUE),
      makeIntegerLearnerParam(id = "res.level", default = 3, tunable = TRUE),
      makeNumericLearnerParam(id = "shift", default = 0.5, lower = 0),
      #makeUntypedLearnerParam(id = "objective", default = "reg:linear", tunable = FALSE),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeDiscreteLearnerParam(id = "dist_measure", default = "dtw", values = c("dtw", "shapelet")),
      makeDiscreteLearnerParam(id = "loss", default = "L2", values = c("L2", "relrmse"))
    ),
    par.vals = list(max_iter = 10, res.level = 3L, shift = 0.5),
    properties = c("numerics"),
    name = "multiResBoost",
    short.name = "mrb",
    package = "stringi",
    note = "A naive implementation"
  )
}

#' @export
trainLearner.fdaregr.multiResBoost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  pars.list = list(...)
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  data = data.matrix(data$data)
  multiResBoost(X = data, y = target, M = pars.list$max_iter, res.level = pars.list$res.levels, shift = pars.list$shift)
}

#' @export
predictLearner.fdaregr.multiResBoost = function(.learner, .model, .newdata, ...) {
  lrnmodel = .model$learner.model
  pred = additivePredict(m = lrnmodel$n_rounds, betas = lrnmodel$betas, intercept = lrnmodel$intercept, models = lrnmodel$models, X = .newdata)
  as.numeric(pred)
}
