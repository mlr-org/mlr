#' @export
#' @inheritParams makeTuneControlMBO
#' @param n.objectives (`integer(1)`)\cr
#'   Number of objectives, i.e. number of [Measure]s to optimize.
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlMBO = function(n.objectives = mbo.control$n.objectives,
  same.resampling.instance = TRUE, impute.val = NULL,
  learner = NULL, mbo.control = NULL, tune.threshold = FALSE, tune.threshold.args = list(),
  continue = FALSE, log.fun = "default", final.dw.perc = NULL, budget = NULL,
  mbo.design = NULL) {

  assertInt(n.objectives, lower = 2L)

  if (!is.null(learner)) {
    learner = checkLearner(learner, type = "regr")
    learner = setPredictType(learner, "se")
  }
  if (is.null(mbo.control)) {
    mbo.control = mlrMBO::makeMBOControl(n.objectives = n.objectives)
    mbo.control = mlrMBO::setMBOControlInfill(mbo.control, crit = mlrMBO::makeMBOInfillCritDIB())
    mbo.control = mlrMBO::setMBOControlMultiObj(mbo.control)
  }
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)

  if (!is.null(budget) && !is.null(mbo.design) && nrow(mbo.design) > budget) {
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      nrow(mbo.design), budget)
  } else if (!is.null(budget)) {
    mbo.control = mlrMBO::setMBOControlTermination(mbo.control, max.evals = budget)
  }

  x = makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    cl = "TuneMultiCritControlMBO", log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  x$mbo.design = mbo.design
  return(x)
}
