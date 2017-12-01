#' @export
makeRLearner.classif.bdk = function() {
  makeRLearnerClassif(
    cl = "classif.bdk",
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
    par.vals = list(keep.data = FALSE),
    properties = c("numerics", "twoclass", "multiclass", "prob"),
    name = "Bi-Directional Kohonen map",
    short.name = "bdk",
    note = "`keep.data` is set to FALSE to reduce memory requirements.",
    callees = c("bdk", "somgrid")
  )
}

#' @export
trainLearner.classif.bdk = function(.learner, .task, .subset, .weights = NULL, xdim, ydim, topo, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  grid = learnerArgsToControl(class::somgrid, xdim, ydim, topo)
  kohonen::bdk(as.matrix(d$data), Y = d$target, grid = grid, ...)
}

#' @export
predictLearner.classif.bdk = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, as.matrix(.newdata), ...)
  if (.learner$predict.type == "response"){
    return(p$prediction)
  } else {
    return(p$unit.predictions[p$unit.classif,])
  }
}

