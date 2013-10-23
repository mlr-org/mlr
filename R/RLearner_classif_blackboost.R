#' @S3method makeRLearner classif.blackboost
makeRLearner.classif.blackboost = function() {
  makeRLearnerClassif(
    cl = "classif.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="family", default=Binomial(), values=list(AdaExp=AdaExp(), Binomial=Binomial())),
      makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
      makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
      makeDiscreteLearnerParam(id="risk", default="inbag", values=c("inbag", "oobag", "none")),
      makeDiscreteLearnerParam(id="teststat", default="quad", values=c("quad", "max")),
      makeDiscreteLearnerParam(id="testtype", default="Bonferroni", values=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id="mincriterion", default=0.95, lower=0, upper=1),
      makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
      makeIntegerLearnerParam(id="minbucket", default=7L, lower=1L),
      makeLogicalLearnerParam(id="stump", default=FALSE),
      makeIntegerLearnerParam(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
      makeIntegerLearnerParam(id="maxsurrogate", default=0L, lower=0L),
      makeIntegerLearnerParam(id="mtry", default=0L, lower=0L),
      makeLogicalLearnerParam(id="savesplitstats", default=TRUE),
      makeIntegerLearnerParam(id="maxdepth", default=0L, lower=0L)
    ),
    par.vals = list(family=Binomial()),
    twoclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

#' @S3method trainLearner classif.blackboost
trainLearner.classif.blackboost = function(.learner, .task, .subset, .weights, mstop, nu, risk, teststat, testtype, mincriterion, maxdepth, ...) {
  ctrl = learnerArgsToControl(boost_control, mstop, nu, risk)
  tc = learnerArgsToControl(ctree_control, teststat, testtype, mincriterion, maxdepth)
  f = getTaskFormula(.task)
  if (!missing(.weights))
    blackboost(f, data=getTaskData(.task, .subset), control=ctrl, tree_controls=tc, weights=.weights, ...)
  else
    blackboost(f, data=getTaskData(.task, .subset), control=ctrl, tree_controls=tc, ...)
}

#' @S3method predictLearner classif.blackboost
predictLearner.classif.blackboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (.learner$predict.type == "prob") {
    y = matrix(0, ncol=2L, nrow=nrow(.newdata))
    colnames(y) = .model$task.desc$class.levels
    y[,1L] = p
    y[,2L] = 1-p
    return(y)
  } else {
    return(p)
  }
}









