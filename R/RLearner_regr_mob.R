#' @S3method makeRLearner regr.mob
makeRLearner.regr.mob = function() {
  makeRLearnerRegr(
    cl = "regr.mob",
    package = "party",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="alpha", default=0.05, lower=0, upper=1),
      makeLogicalLearnerParam(id="bonferroni", default=TRUE),
      makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
      makeNumericLearnerParam(id="trim", default=0.1, lower=0, upper=1),
      makeLogicalLearnerParam(id="breakties", default=FALSE),
      makeDiscreteLearnerParam(id="model", default=glinearModel,
        values=list(glinearModel=glinearModel, linearModel=linearModel)),
      makeUntypedLearnerParam(id="part.feats"),
      makeUntypedLearnerParam(id="term.feats")
    ),
    par.vals = list(),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @S3method trainLearner regr.mob
trainLearner.regr.mob = function(.learner, .task, .subset, .weights, alpha, bonferroni, minsplit, 
  trim, breakties, part.feats, term.feats, ...) {
  
  cntrl = learnerArgsToControl(mob_control, alpha, bonferroni, minsplit, trim, breakties)
  
  feats = getTaskFeatureNames(.task)
  # FIXME document stuff
  # FIXME think about these defaults, also ask julia
  if (missing(part.feats))
    part.feats = feats
  if (missing(term.feats))
    term.feats = feats
  
  target = .task$task.desc$target
  f = as.formula(paste(target, "~", collapse(term.feats, sep=" + "), "|", collapse(part.feats, sep=" + ")))
  
  if (missing(.weights)) {
    mob(f, data=getTaskData(.task, .subset), control = cntrl, ...)
  } else  {
    mob(f, data=getTaskData(.task, .subset), control = cntrl, weights=.weights, ...)
  }
}

#' @S3method predictLearner regr.mob
predictLearner.regr.mob = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)
}
