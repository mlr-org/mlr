#' @export
makeRLearner.classif.earth = function() {
  makeRLearnerClassif(
    cl = "classif.earth",
    package = "earth",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "trace", default = 0, upper = 10, tunable = FALSE),
      makeLogicalLearnerParam(id = "keepxy",default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "degree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "penalty"),
      makeIntegerLearnerParam(id = "nk", lower = 0L),
      makeNumericLearnerParam(id = "thres", default = 0.001),
      makeIntegerLearnerParam(id = "minspan", default = 0L),
      makeIntegerLearnerParam(id = "endspan", default = 0L),
      makeNumericLearnerParam(id = "newvar.penalty", default = 0),
      makeIntegerLearnerParam(id = "fast.k", default = 20L, lower = 0L),
      makeNumericLearnerParam(id = "fast.beta", default = 1),
      makeDiscreteLearnerParam(id = "pmethod", default = "cv",
                               values = c("backward", "none", "exhaustive", "forward", "seqrep", "cv")),
      makeIntegerLearnerParam(id = "nprune"),
      makeIntegerLearnerParam(id = "ncross", default = 1L),
      makeIntegerLearnerParam(id = "nfold", default = 1L),
      makeLogicalLearnerParam(id = "stratify",default = TRUE),
      makeUntypedLearnerParam(id = "linpreds",default = FALSE),
      makeDiscreteLearnerParam("link", values = c("logit", "probit"),
                               default = "logit"),
      makeNumericLearnerParam(id = "maxit", default = 25L, tunable = FALSE),
      makeFunctionLearnerParam(id = "allowed")
    ),
    properties = c("twoclass", "numerics", "factors", "prob","weights"),
    name = "Flexible Discriminant Analysis",
    short.name = "fda",
    note = "This learner performs flexible discriminant analsis using the earth algorithm."
  )
}

#' @export
trainLearner.classif.earth = function(.learner, .task, .subset, .weights = NULL, link = "logit", maxit = 25L, ...) {
  f = getTaskFormula(.task)
  earth::earth(f, data = getTaskData(.task, .subset), glm = list(family = binomial(link = link) , maxit = maxit), ...)
}

#' @export
predictLearner.classif.earth = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$factor.levels[[1]]
  if (.learner$predict.type == "prob") {
    p = setColNames(cbind(1 - p, p), levs)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
  return(p)
}

