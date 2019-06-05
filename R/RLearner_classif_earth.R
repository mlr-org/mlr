#' @export
makeRLearner.classif.earth = function() {
  makeRLearnerClassif(
    cl = "classif.earth",
    package = c("!earth", "stats"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "trace", default = 0, upper = 5, tunable = FALSE),
      makeLogicalLearnerParam(id = "keepxy", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "degree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "penalty"),
      makeIntegerLearnerParam(id = "nk", lower = 1L),
      makeNumericLearnerParam(id = "thresh", default = 0.001),
      makeIntegerLearnerParam(id = "minspan", default = 0L),
      makeIntegerLearnerParam(id = "endspan", default = 0L),
      makeNumericLearnerParam(id = "newvar.penalty", default = 0),
      makeIntegerLearnerParam(id = "fast.k", default = 20L, lower = 0L),
      makeNumericLearnerParam(id = "fast.beta", default = 1),
      makeDiscreteLearnerParam(id = "pmethod", default = "backward",
        values = c("backward", "none", "exhaustive", "forward", "seqrep", "cv")),
      makeIntegerLearnerParam(id = "nprune"),
      makeIntegerLearnerParam(id = "ncross", default = 1L),
      makeIntegerLearnerParam(id = "nfold", default = 0L, requires = quote(pmethod == "cv")),
      makeLogicalLearnerParam(id = "stratify", default = TRUE, requires = quote(nfold > 1L)),
      makeUntypedLearnerParam(id = "linpreds", default = FALSE),
      makeDiscreteLearnerParam("link", values = c("logit", "probit"),
        default = "logit"),
      makeNumericLearnerParam(id = "maxit", default = 25L, tunable = TRUE),
      makeFunctionLearnerParam(id = "allowed"),
      makeNumericLearnerParam(id = "Adjust.endspan", default = 2, tunable = TRUE),
      makeLogicalLearnerParam(id = "Force.weights", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "Use.beta.cache", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "Force.xtx.prune", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "Get.leverages", default = TRUE, tunable = FALSE),
      makeNumericLearnerParam(id = "Exhaustive.tol", default = 1e-10, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "Flexible Discriminant Analysis",
    short.name = "fda",
    note = "This learner performs flexible discriminant analysis using the earth algorithm. na.action is set to na.fail and only this is supported.",
    callees = c("earth", "glm", "glm.control", "binomial")
  )
}

#' @export
trainLearner.classif.earth = function(.learner, .task, .subset, .weights = NULL, link = "logit", maxit = 25L, ...) {
  f = getTaskFormula(.task)
  earth::earth(f, data = getTaskData(.task, .subset), weights = .weights, glm = list(family = binomial(link = link), maxit = maxit), ...)
}

#' @export
predictLearner.classif.earth = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    if (length(levs) == 2) p = propVectorToMatrix(p, levs)
  } else {
    if (length(levs) == 2) {
      p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    } else {
      p = as.factor(predict(.model$learner.model, newdata = .newdata, type = "class", ...))
    }
    p = unname(p)
  }
  return(p)
}
