#' @export
makeRLearner.regr.bdk = function() {
  makeRLearnerRegr(
    cl = "regr.bdk",
    package = c("kohonen", "class"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "xdim", default = 8L, lower = 1L),
      makeIntegerLearnerParam(id = "ydim", default = 6L, lower = 1L),
      makeDiscreteLearnerParam(id = "topo", default = "rectangular", values = c("rectangular", "hexagonal")),
      makeIntegerLearnerParam(id = "rlen", default = 100L, lower = 1L),
      makeNumericVectorLearnerParam(id = "alpha", default = c(0.05, 0.01), len = 2L),
      makeNumericVectorLearnerParam(id = "radius"),
      makeNumericLearnerParam(id = "xweight", default = 0.75, lower = 0),
      makeLogicalLearnerParam(id = "contin", tunable = FALSE),
      makeLogicalLearnerParam(id = "toroidal", default = FALSE),
      makeDiscreteLearnerParam(id = "n.hood", values = c("circular", "square")),
      makeLogicalLearnerParam(id = "keep.data", default = TRUE, tunable = FALSE)
    ),
    properties = "numerics",
    par.vals = list(keep.data = FALSE),
    name = "Bi-Directional Kohonen map",
    short.name = "bdk",
    note = "`keep.data` is set to FALSE to reduce memory requirements.",
    callees = c("bdk", "somgrid")
  )
}

#' @export
trainLearner.regr.bdk = function(.learner, .task, .subset, .weights = NULL, xdim, ydim, topo, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  grid = learnerArgsToControl(class::somgrid, xdim, ydim, topo)
  kohonen::bdk(as.matrix(d$data), Y = d$target, grid = grid, ...)
}

#' @export
predictLearner.regr.bdk = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, as.matrix(.newdata), ...)$prediction[, 1L]
}
