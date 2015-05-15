#FIXME: docu check

#FIXME: export when mbo is on cran

# @param learner [\code{\link{Learner}}]\cr
#   Regression learner to model performance landscape.
# @param continue [\code{logical(1)}]\cr
#   Resume calculation from previous run using \code[mboContinue}?
#   Requires \dQuote{save.file.path) to be set.
# @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#   Control object for model-based optimization tuning.
# @export
# @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = NULL, learner, mbo.control,
  tune.threshold = FALSE, tune.threshold.args = list(), continue = FALSE, log.fun = NULL, budget = 30L) {

  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)

  if (mbo.control$init.design.points > budget)
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      mbo.control$init.design.points, budget)
  if (mbo.control$iters + mbo.control$init.design.points != budget) {
    warningf("Considering the given budget, the number of iterations (iters = %i) was changed to %i.",
      mbo.control$iters, budget - mbo.control$init.design.points)
    mbo.control$iters = budget - mbo.control$init.design.points
  }

  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    cl = "TuneControlMBO", log.fun = log.fun, budget = budget)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  return(x)
}
