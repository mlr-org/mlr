#' @export
makeRLearner.classif.rotationForest = function() {
  makeRLearnerClassif(
    cl = "classif.rotationForest",
    package = "rotationForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "K", default = 3L, lower = 1L),
      makeIntegerLearnerParam(id = "L", default = 10L, lower = 1L)
    ),
    properties = c("twoclass", "numerics", "factors", "ordered", "prob"),
    name = "Rotation Forest",
    short.name = "rotationForest",
    note = "factors must be transformed to dummy variables"
    )
}

#' @export
trainLearner.classif.rotationForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  df = getTaskData(.task, .subset, target.extra = TRUE)
  features = df$data
  #rotationForest needs 0-1 coding
  target = as.factor(ifelse(df$target == .task$task.desc$positive, 1L, 0L))
  rotationForest::rotationForest(x = features, y = target, ...)
}

#' @export
predictLearner.classif.rotationForest = function(.learner, .model, .newdata, ...) {
  features = .newdata[, names(.newdata) == .model$features]
  p = predict(.model$learner.model, newdata = features, all = FALSE, ...)
  if(.learner$predict.type == "prob"){
    p0 = 1 - p
    p = matrix(c(p, p0), ncol = 2L)
    colnames(p) = c(.model$task.desc$positive, .model$task.desc$negative)  
  }else{
    p = as.factor(ifelse(p > 0.5, .model$task.desc$positive, .model$task.desc$negative))
  }
  return(p)
}