#FIXME: docu check

#FIXME export when mbo is on cran

# @param learner [\code{\link{Learner}}] \cr
#   Regression learner to model performance landscape.
# @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#   Control object for model-based optimization tuning.
# @export
# @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance = TRUE, learner, mbo.control) {
  assertClass(learner, classes = "Learner")
  checkArg(mbo.control, "MBOControl")
  x = makeTuneControl(same.resampling.instance = same.resampling.instance, start = list(), cl = "TuneControlMBO")
  x$learner = learner
  x$mbo.control = mbo.control
  return(x)
}
