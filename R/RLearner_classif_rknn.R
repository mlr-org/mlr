#' @export
makeRLearner.classif.rknn = function(){
  makeRLearnerClassif(
    cl = "classif.rknn",
    package = "rknn",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L, upper = 98L),
      makeIntegerLearnerParam(id = "r", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "seed", lower = 1L),
      makeUntypedLearnerParam(id = "cluster", default = NULL)
    ),
    # rknn can't handle unordered factors or return probs
    properties = c("twoclass", "multiclass", "numerics", "ordered"),
    name = "Random k-Nearest-Neighbors",
    short.name = "rknn",
    note = "k restricted to < 99 as the code allocates arrays of static size",
    callees = "rknn"
  )
}

#' @export
trainLearner.classif.rknn = function(.learner, .task, .subset, .weights = NULL,  ...){
  z = getTaskData(.task, .subset, target.extra = TRUE)
  c(list(data = z$data, y = z$target), list(...))
}

#' @export
predictLearner.classif.rknn = function(.learner, .model, .newdata, ...){
  args = .model$learner.model
  args$newdata = .newdata
  do.call(rknn::rknn, args)$pred
}
