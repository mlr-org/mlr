#' @S3method makeRLearner classif.plr
makeRLearner.classif.plr = function() {
  makeRLearnerClassif(
    cl = "classif.plr",
    package = "stepPlr",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-4, lower = 0),
      makeNumericLearnerParam(id = "cp", default = 2)
    ),
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

#' @S3method trainLearner classif.plr
trainLearner.classif.plr = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  if (missing(.weights)) {
    plr(x = d$data, y = d$target, ...)
  } else {
    plr(x = d$data, y = d$target, weights = .weights, ...)
  }
}

#' @S3method predictLearner classif.plr
predictLearner.classif.plr = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newx = .newdata, type = "response", ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if(.learner$predict.type == "prob"){
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = levs
    y[,1L] = 1-p
    y[,2L] = p
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    names(p) = NULL
    return(p)
  }
}
