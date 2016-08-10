#' @export
makeRLearner.classif.obliqueRF = function() {
  makeRLearnerClassif(
    cl = "classif.obliqueRF",
    package = "!obliqueRF",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeDiscreteLearnerParam(id = "training_method", default = "ridge",
       values = c("ridge", "ridge_slow", "pls", "svm", "log", "rnd"))
    ),
    properties = c("twoclass", "numerics", "factors", "ordered", "prob"),
    name = "Oblique Random Forest",
    short.name = "obliqueRF",
    note = ""
    )
}

#' @export
trainLearner.classif.obliqueRF = function(.learner, .task, .subset, .weights = NULL, ...) {
  df = getTaskData(.task, .subset, target.extra = TRUE)
  features = as.matrix(df$data)
  target = ifelse(df$target == .task$task.desc$positive, 1, 0)
  obliqueRF::obliqueRF(x = features, y = target, bImportance = FALSE,
   bProximity = FALSE, verbose = FALSE, ...)
}

#' @export
predictLearner.classif.obliqueRF = function(.learner, .model, .newdata, ...) {
  features = .newdata[, .model$features]
  features = data.frame(features)
  p = predict(.model$learner.model, newdata = features, type = .learner$predict.type, proximity = FALSE, ...)
  if(.learner$predict.type == "prob"){
    colnames(p) = c(.model$task.desc$negative, .model$task.desc$positive)
  }else{
    p = as.factor(p)
    p = as.factor(ifelse(p == 1L, .model$task.desc$positive, .model$task.desc$negative))
  }
  return(p)
}