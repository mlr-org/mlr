#' @S3method makeRLearner classif.ctree
makeRLearner.classif.ctree = function() {
  makeRLearnerClassif(
    cl = "classif.ctree",
    package = "party",
    par.set = makeParamSet(
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
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

#' @S3method trainLearner classif.ctree
trainLearner.classif.ctree = function(.learner, .task, .subset, .weights, teststat, testtype,
  mincriterion, minsplit, minbucket, stump, nresample, maxsurrogate, mtry,
  savesplitstats, maxdepth, ...) {

  ctrl = learnerArgsToControl(ctree_control, teststat, testtype, mincriterion, minsplit,
    minbucket, stump, nresample, maxsurrogate, mtry, savesplitstats, maxdepth)
  f = getTaskFormula(.task)
  ctree(f, data=getTaskData(.task, .subset), controls=ctrl, ...)
}

#' @S3method predictLearner classif.ctree
predictLearner.classif.ctree = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "prob") {
    m = .model$learner.model
    p = treeresponse(m, newdata=.newdata, ...)
    p = do.call(rbind, p)
    rownames(p) = NULL
    colnames(p) = m@responses@levels[[.model$task.desc$target]]
    return(p)
  } else {
    predict(.model$learner.model, newdata=.newdata, ...)
  }
}
