## DOES NOT WORK:

#'@export
makeRLearner.fcregr.thief = function() {
  makeRLearnerRegr(
    cl = "fcregr.thief",
    package = "thief",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "h", lower = 1L, default = 1L),
      makeDiscreteLearnerParam(id = "m", values = c(1L, 4L, 7L, 12L, 24L, 48L, 52L, 168L, 336L, 8760L,
                                                    17520L), default = 1L),
      makeDiscreteLearnerParam(id = "comb", values = c("struc","mse","old","bu","shr","sam"),
                               default = "struc"),
      makeDiscreteLearnerParam(id = "usemodel", values = c("ets","arima","theta","naive","snaive"),
                               default = "ets"),
      makeUntypedLearnerParam(id = "forecastfunction", default = NULL)
    ),
    properties = c("numerics","ts"),
    name = "Temporal hierarchical forecasting",
    short.name = "thief",
    note = ""
  )
}
#'@export
trainLearner.fcregr.thief = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  thief::thief(y = data$target, ...)
}

#'@export
predictLearner.fcregr.thief = function(.learner, .model, .newdata, ...){
  as.numeric(.model$learner.model$mean)
}

