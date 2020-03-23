#' @export
makeRLearner.regr.mob = function() {
  makeRLearnerRegr(
    cl = "regr.mob",
    package = c("party", "modeltools"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 0.05, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "bonferroni", default = TRUE),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeNumericLearnerParam(id = "trim", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "breakties", default = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "glinearModel", values = list("glinearModel", "linearModel")),
      makeUntypedLearnerParam(id = "part.feats"),
      makeUntypedLearnerParam(id = "term.feats")
    ),
    par.vals = list(),
    properties = c("numerics", "factors", "weights"),
    name = "Model-based Recursive Partitioning  Yielding a Tree with Fitted Models Associated with each Terminal Node",
    short.name = "mob",
    callees = c("mob", "mob_control")
  )
}

#' @export
trainLearner.regr.mob = function(.learner, .task, .subset, .weights = NULL, alpha, bonferroni, minsplit,
  trim, breakties, verbose, part.feats, term.feats, model, ...) {

  cntrl = learnerArgsToControl(party::mob_control, alpha, bonferroni, minsplit, trim, breakties, verbose)

  feats = getTaskFeatureNames(.task)
  # FIXME: document stuff
  # FIXME: think about these defaults, also ask julia
  if (missing(part.feats)) {
    part.feats = feats
  }
  if (missing(term.feats)) {
    term.feats = feats
  }

  target = getTaskTargetNames(.task)
  f = as.formula(stri_paste(target, "~", collapse(term.feats, sep = " + "), "|", collapse(part.feats, sep = " + "), sep = " "))

  args = list(f, data = getTaskData(.task, .subset), control = cntrl, ...)
  if (!is.null(.weights)) {
    args$weights = .weights
  }
  if (!missing(model)) {
    if (is.character(model)) {
      args$model = getFromNamespace(model, "mda")
    } else {
      args$model = model # this allows to set the model if on.par.out.of.bounds is set to "warn" or "quiet"
    }
  }

  model = do.call(party::mob, args)

  # sometimes mob fails to fit a model but does not signal an exception.
  if (anyMissing(coef(model))) {
    stop("Failed to fit party::mob. Some coefficients are estimated as NA")
  }
  model
}

#' @export
predictLearner.regr.mob = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
