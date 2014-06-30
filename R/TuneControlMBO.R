#FIXME: docu check

#FIXME export when mbo is on cran

# @param learner [\code{\link{Learner}}] \cr
#   Regression learner to model performance landscape.
# @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#   Control object for model-based optimization tuning.
# @export
# @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = Inf, learner, mbo.control) {
  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, cl = "TuneControlMBO")
  x$learner = learner
  x$mbo.control = mbo.control
  return(x)
}
