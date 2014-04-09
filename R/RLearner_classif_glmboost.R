#' @S3method makeRLearner classif.glmboost
makeRLearner.classif.glmboost = function() {
  makeRLearnerClassif(
    cl = "classif.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="family", default=Binomial(), values=list(AdaExp=AdaExp(), Binomial=Binomial())),
      makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
      makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
      makeLogicalLearnerParam(id="center", default=FALSE)
    ),
    par.vals = list(family=Binomial()),
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

#' @S3method trainLearner classif.glmboost
trainLearner.classif.glmboost = function(.learner, .task, .subset, .weights, mstop, nu, risk, ...) {
  f = getTaskFormula(.task)
  ctrl = learnerArgsToControl(boost_control, mstop, nu, risk)
  if (missing(.weights)) {
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, , ...)
  } else  {
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, weights=.weights, ...)
  }
}

#' @S3method predictLearner classif.glmboost
predictLearner.classif.glmboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (.learner$predict.type  == "prob") {
    p = p[,1L]
    y = matrix(0, ncol=2L, nrow=nrow(.newdata))
    colnames(y) <- .model$task.desc$class.levels
    y[,1L] = p
    y[,2L] = 1-p
    return(y)
  } else {
    return(p)
  }
}
