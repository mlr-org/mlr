#' @export
makeRLearner.regr.rknn = function(){
  makeRLearnerRegr(
    cl = "regr.rknn",
    package = "rknn",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "r", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "seed", default = 2015L, lower = 1L),
      makeUntypedLearnerParam(id = "cluster", default = NULL)
    ),
    # rknn can't handle factors or return probs
    properties = c("numerics", "ordered"),
    name = "Random k-Nearest-Neighbors",
    short.name = "rknn",
    note = "",
    callees = "rknnReg"
  )
}

#' @export
trainLearner.regr.rknn = function(.learner, .task, .subset, .weights = NULL,  ...){
  z = getTaskData(.task, .subset, target.extra = TRUE)
  c(list(data = z$data, y = z$target), list(...))
}

#' @export
predictLearner.regr.rknn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$newdata = .newdata
  do.call(rknn::rknnReg, args)$pred
}
