#' @export
makeRLearner.classif.evtree = function() {
  makeRLearnerClassif(
    cl = "classif.evtree",
    package = "evtree",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "minbucket", lower = 1L, default = 7L),
      makeIntegerLearnerParam(id = "minsplit", lower = 1L, default = 20L),
      makeIntegerLearnerParam(id = "maxdepth", lower = 1L, default = 9L),
      makeIntegerLearnerParam(id = "niterations", lower = 1L, default = 10000L, tunable = FALSE),
      makeIntegerLearnerParam(id = "ntrees", lower = 2L, default = 100L),
      makeNumericLearnerParam(id = "alpha", lower = 0, default = 1),
      makeNumericLearnerParam(id = "pmutatemajor", lower = 0, default = 20),
      makeNumericLearnerParam(id = "pmutateminor", lower = 0, default = 20),
      makeNumericLearnerParam(id = "pcrossover", lower = 0, default = 20),
      makeNumericLearnerParam(id = "psplit", lower = 0, default = 20),
      makeNumericLearnerParam(id = "pprune", lower = 0, default = 20)
    ),
    properties = c("twoclass", "multiclass", "prob", "factors", "numerics", "ordered", "weights"),
    par.vals = list(),
    name = "Evolutionary learning of globally optimal trees",
    short.name = "evtree",
    note = "`pmutatemajor`, `pmutateminor`, `pcrossover`, `psplit`, and `pprune`,
      are scaled internally to sum to 100.",
    callees = c("evtree", "evtree.control")
  )
}

#' @export
trainLearner.classif.evtree = function(.learner, .task, .subset,
  .weights = NULL, pmutatemajor, pmutateminor, pcrossover, psplit,
  pprune, seed, ...) {

  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  defaults = getDefaults(getParamSet(.learner))

  if (missing(pmutatemajor)) pmutatemajor = defaults$pmutatemajor
  if (missing(pmutateminor)) pmutateminor = defaults$pmutateminor
  if (missing(pcrossover)) pcrossover = defaults$pcrossover
  if (missing(psplit)) psplit = defaults$psplit
  if (missing(pprune)) pprune = defaults$pprune

  evtree::evtree(f, data = d, operatorprob = list(pmutatemajor, pmutateminor, pcrossover, psplit, pprune),
    weights = .weights, ...)
}

#' @export
predictLearner.classif.evtree = function(.learner, .model, .newdata, ...) {
  colnames(.newdata) = attr(.model$learner.model$terms, "term.labels")
  if (.learner$predict.type == "prob") {
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, ...)
  }
}
