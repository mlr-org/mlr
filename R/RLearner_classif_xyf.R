#' @export
makeRLearner.classif.xyf = function() {
  makeRLearnerClassif(
    cl = "classif.xyf",
    package = c("kohonen", "class"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "xdim", default = 8L, lower = 1L),
      makeIntegerLearnerParam(id = "ydim", default = 6L, lower = 1L),
      makeDiscreteLearnerParam(id = "topo", default = "rectangular", values = c("rectangular", "hexagonal")),
      makeIntegerLearnerParam(id = "rlen", default = 100L, lower = 1L),
      makeNumericVectorLearnerParam(id = "alpha", default = c(0.05, 0.01), len = 2L),
      makeNumericVectorLearnerParam(id = "radius"),
      makeLogicalLearnerParam(id = "toroidal", default = FALSE),
      makeDiscreteLearnerParam(id = "neighbourhood.fct", values = c("bubble", "gaussian"), default = "bubble"),
      makeDiscreteLearnerParam(id = "dist.fcts", values = c("sumofsquares", "euclidean", "manhattan", "tanimoto"), default = "sumofsquares")
    ),
    properties = c("numerics", "twoclass", "multiclass", "prob"),
    name = "X-Y fused self-organising maps",
    short.name = "xyf",
    callees = c("xyf", "somgrid")
  )
}

#' @export
trainLearner.classif.xyf = function(.learner, .task, .subset, .weights = NULL, xdim, ydim, topo, neighbourhood.fct, toroidal, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  grid = learnerArgsToControl(kohonen::somgrid, xdim, ydim, topo, neighbourhood.fct, toroidal)
  kohonen::supersom(list(X = as.matrix(d$data), Y = d$target), grid = grid, keep.data = TRUE, ...)
}

#' @export
predictLearner.classif.xyf = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, list(X = as.matrix(.newdata)), ...)
  if (.learner$predict.type == "response"){
    return(p$predictions[[2]])
  } else {
    return(p$unit.predictions[[2]][p$unit.classif,])
  }
}
