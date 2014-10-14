#' @export
makeRLearner.regr.mob = function() {
  makeRLearnerRegr(
    cl = "regr.mob",
    package = "party",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 0.05, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "bonferroni", default = TRUE),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeNumericLearnerParam(id = "trim", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "breakties", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = modeltools::glinearModel,
        values = list(glinearModel = modeltools::glinearModel, linearModel = modeltools::linearModel)),
      makeUntypedLearnerParam(id = "part.feats"),
      makeUntypedLearnerParam(id = "term.feats")
    ),
    par.vals = list(),
    properties = c("numerics", "factors", "weights"),
    name = "Model-based Recursive Partitioning  Yielding a Tree with Fitted Models Associated with each Terminal Node",
    short.name = "mob",
    note = ""
  )
}

#' @export
trainLearner.regr.mob = function(.learner, .task, .subset, .weights = NULL, alpha, bonferroni, minsplit,
  trim, breakties, part.feats, term.feats, ...) {

  cntrl = learnerArgsToControl(party::mob_control, alpha, bonferroni, minsplit, trim, breakties)

  feats = getTaskFeatureNames(.task)
  # FIXME: document stuff
  # FIXME: think about these defaults, also ask julia
  if (missing(part.feats))
    part.feats = feats
  if (missing(term.feats))
    term.feats = feats

  target = .task$task.desc$target
  f = as.formula(paste(target, "~", collapse(term.feats, sep = " + "), "|", collapse(part.feats, sep = " + ")))

  if (is.null(.weights)) {
    party::mob(f, data = getTaskData(.task, .subset), control = cntrl, ...)
  } else  {
    party::mob(f, data = getTaskData(.task, .subset), control = cntrl, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.mob = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
