#FIXME: docu check

#FIXME: export when mbo is on cran

# @param learner [\code{\link{Learner}}]\cr
#   Regression learner to model performance landscape.
# @param continue [\code{logical(1)}]\cr
#   Resume calculation from previous run using \code{mboContinue}?
#   Requires \dQuote{save.file.path) to be set.
#   Note that the \code{OptPath} in the \code{OptResult} will only include
#   the evaluations after the continuation.
#   The complete \code{OptPath} will be found in \code{$mbo.result$opt.path}.
# @param mbo.control [\code{\link[mlrMBO]{MBOControl}}] \cr
#   Control object for model-based optimization tuning.
# @param mbo.keep.result [\code{logical(1)}] \cr
#    Should the \code{MBOResult} be stored in the result.
# @param mbo.design [\code{data.frame} | NULL]\cr
#   Initial design as data frame.
#   If the parameters have corresponding trafo functions,
#   the design must not be transformed before it is passed!
#   If \code{NULL}, one is constructed from the settings in \code{mbo.control}.
# @export
# @rdname TuneControl
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = NULL,
  learner, mbo.control, tune.threshold = FALSE, tune.threshold.args = list(),
  continue = FALSE, log.fun = NULL, final.dw.perc = NULL, budget = NULL, mbo.keep.result = FALSE, mbo.design = NULL) {

  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)
  assertFlag(mbo.keep.result)

  if(!is.null(budget) && !is.null(mbo.design) && nrow(mbo.design) > budget)
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      nrow(mbo.design), budget)
  else if (!is.null(budget)) {
    if (!is.null(mbo.control$stop.conds))
      warning("The mbo.control object already has a stopping condition. However we add another one respecting the budget.", mbo.control$init.design.points, budget)
    setMBOControlTermination = get("setMBOControlTermination", envir = getNamespace("mlrMBO")) # FIXME: Remove if mlrMBO hits CRAN
    mbo.control = setMBOControlTermination(mbo.control, max.evals = budget)
  }

  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    cl = "TuneControlMBO", log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  x$mbo.keep.result = mbo.keep.result
  x$mbo.design = mbo.design
  return(x)
}
