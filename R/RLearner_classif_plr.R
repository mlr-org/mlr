#' @export
makeRLearner.classif.plr = function() {
  makeRLearnerClassif(
    cl = "classif.plr",
    package = "stepPlr",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-4, lower = 0),
      makeDiscreteLearnerParam(id = "cp.type", values = c("bic", "aic"), default = "bic"),
      makeNumericLearnerParam(id = "cp", lower = 0, default = 2)
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Logistic Regression with a L2 Penalty",
    short.name = "plr",
    note = "AIC and BIC penalty types can be selected via the new parameter `cp.type`.",
    callees = "plr"
  )
}

#' @export
trainLearner.classif.plr = function(.learner, .task, .subset, .weights = NULL, cp.type, cp, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  # cp.type has preference
  if (!missing(cp.type)) {
    cp2 = cp.type
  } else if (!missing(cp)) {
    cp2 = cp
  } else {
    cp2 = NULL
  }
  args = list(x = d$data, y = d$target)
  args$cp = cp2
  if (!is.null(.weights)) {
    args$weights = .weights
  }
  args = c(args, list(...))
  do.call(stepPlr::plr, args)
}

#' @export
predictLearner.classif.plr = function(.learner, .model, .newdata, ...) {
  p = stepPlr::predict.plr(.model$learner.model, newx = .newdata, type = "response", ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if (.learner$predict.type == "prob") {
    y = propVectorToMatrix(p, levs)
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    names(p) = NULL
    return(p)
  }
}
