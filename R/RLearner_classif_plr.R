#' @S3method makeRLearner classif.plr
makeRLearner.classif.plr = function() {
  makeRLearnerClassif(
    cl = "classif.plr",
    package = "stepPlr",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-4, lower = 0),
      makeDiscreteLearnerParam(id = "cp.type", values = c("bic", "aic"), default = "bic"),
      makeNumericLearnerParam(id = "cp", lower = 0, default = 2)
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
trainLearner.classif.plr = function(.learner, .task, .subset, .weights, cp.type, cp,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  # cp.type has preference
  if (!missing(cp.type))
    cp2 = cp.type
  else if (!missing(cp))
    cp2 = cp
  else
    cp2 = NULL
  args = list(x = d$data, y = d$target)
  args$cp = cp2
  if (!missing(.weights))
    args$weights = .weights
  args = c(args, list(...))
  do.call(plr, args)
}

#' @S3method predictLearner classif.plr
predictLearner.classif.plr = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newx = .newdata, type = "response", ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if(.learner$predict.type == "prob"){
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = levs
    y[, 1L] = 1 - p
    y[, 2L] = p
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    names(p) = NULL
    return(p)
  }
}
