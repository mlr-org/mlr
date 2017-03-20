#' @export
makeRLearner.regr.xyf = function() {
  makeRLearnerRegr(
    cl = "regr.xyf",
    package = "kohonen",
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
    properties = c("numerics"),
    name = "X-Y fused self-organising maps",
    short.name = "xyf"
  )
}

#' @export
trainLearner.regr.xyf = function(.learner, .task, .subset, .weights = NULL, xdim, ydim, topo, neighbourhood.fct, toroidal, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  grid = learnerArgsToControl(kohonen::somgrid, xdim, ydim, topo, neighbourhood.fct, toroidal)
  kohonen::xyf(X = as.matrix(d$data), Y = matrix(d$target, ncol=1), grid = grid, keep.data = TRUE, ...)
}

#' @export
predictLearner.regr.xyf = function(.learner, .model, .newdata, ...) {
  as.vector(predict(.model$learner.model, as.matrix(.newdata), whatmap = 1, ...)$predictions[[2]])
}
