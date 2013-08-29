#FIXME: docu check

#' @param learner [\code{\link[mlr]{Learner}}] \cr
#'   Regression learner to model performance landscape.  
#' @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#'   Control object for model-based optimization tuning.  
#' @export
#' @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance=TRUE, learner, mbo.control) {
  checkArg(learner, "Learner")
  checkArg(mbo.control, "MBOControl")
  x = makeTuneControl(same.resampling.instance=same.resampling.instance, start=list(), cl="TuneControlMBO")
  x$learner = learner
  x$mbo.control = mbo.control
  return(x)
}

