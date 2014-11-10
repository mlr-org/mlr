#FIXME: docu check

#FIXME: export when mbo is on cran

# @param learner [\code{\link{Learner}}] \cr
#   Regression learner to model performance landscape.
# @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#   Control object for model-based optimization tuning.
# @export
# @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = NULL, learner, mbo.control, tune.threshold = FALSE, log.fun = NULL) {
  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, cl = "TuneControlMBO", log.fun = log.fun)
  x$learner = learner
  x$mbo.control = mbo.control
  return(x)
}
