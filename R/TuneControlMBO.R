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
  tune.threshold = FALSE, tune.threshold.args = list(), continue = FALSE, log.fun = NULL, final.dw.perc = NULL) {

  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)
  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    cl = "TuneControlMBO", log.fun = log.fun, final.dw.perc = final.dw.perc)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  return(x)
}
