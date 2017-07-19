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
      makeNumericLearnerParam(id = "xweight", default = 0.5, lower = 0),
      makeLogicalLearnerParam(id = "contin"),
      makeLogicalLearnerParam(id = "toroidal", default = FALSE),
      makeDiscreteLearnerParam(id = "n.hood", values = c("circular", "square"))
    ),
    properties = c("numerics", "twoclass", "multiclass", "prob"),
    name = "X-Y fused self-organising maps",
    short.name = "xyf",
    callees = c("xyf", "somgrid")
  )
}

#' @export
trainLearner.classif.xyf = function(.learner, .task, .subset, .weights = NULL, xdim, ydim, topo, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  grid = learnerArgsToControl(class::somgrid, xdim, ydim, topo)
  kohonen::xyf(as.matrix(d$data), Y = d$target, grid = grid, keep.data = FALSE, ...)
}

#' @export
predictLearner.classif.xyf = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, as.matrix(.newdata), ...)
  if (.learner$predict.type == "response"){
    return(p$prediction)
  } else {
    return(p$unit.predictions[p$unit.classif,])
  }
}
