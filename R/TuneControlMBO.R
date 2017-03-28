#' @title Create control object for hyperparameter tuning with MBO.
#'
#' @description
#' Model-based / Bayesian optimization with the function
#' \code{\link[mlrMBO]{mbo}} from the \pkg{mlrMBO} package.
#' Please refer to \url{https://github.com/mlr-org/mlrMBO} for further info.
#'
#' @inherit TuneControl
#' @param budget [\code{integer(1)}]\cr
#'   Maximum budget for tuning. This value restricts the number of function evaluations.
#' @param learner [\code{\link{Learner}} | \code{NULL}]\cr
#'   The surrogate learner: A regression learner to model performance landscape.
#'  For the default, \code{NULL}, \pkg{mlrMBO} will automatically create a suitable learner based on the rules described in \code{\link[mlrMBO]{makeMBOLearner}}.
#' @param continue [\code{logical(1)}]\cr
#'   Resume calculation from previous run using \code{\link[mlrMBO]{mboContinue}}?
#'   Requires \dQuote{save.file.path} to be set.
#'   Note that the \code{\link[ParamHelpers]{OptPath}} in the \code{\link[mlrMBO]{OptResult}}
#'   will only include the evaluations after the continuation.
#'   The complete \code{\link{OptPath}} will be found in the slot \code{$mbo.result$opt.path}.
#' @param mbo.control [\code{\link[mlrMBO]{MBOControl}} | \code{NULL}]\cr
#'   Control object for model-based optimization tuning.
#'   For the default, \code{NULL}, the control object will be created with all the defaults as described in \code{\link[mlrMBO]{makeMBOControl}}.
#' @param mbo.keep.result [\code{logical(1)}] \cr
#'    Should the \code{\link[mlrMBO]{MBOSingleObjResult}} be stored in the result?
#'    Default is \code{FALSE}.
#' @param mbo.design [\code{data.frame} | \code{NULL}]\cr
#'   Initial design as data frame.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   For the default, \code{NULL}, a default design is created like described in \code{\link[mlrMBO]{mbo}}.
#' @return [\code{\link{TuneControlMBO}}]
#' @references Bernd Bischl, Jakob Richter, Jakob Bossek, Daniel Horn, Janek Thomas and Michel Lang; mlrMBO: A Modular Framework for Model-Based Optimization of Expensive Black-Box Functions, Preprint: \url{https://arxiv.org/abs/1703.03373} (2017).
#' @aliases TuneControlMBO
#' @family tune
#' @export
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = NULL,
  learner = NULL, mbo.control = NULL, tune.threshold = FALSE, tune.threshold.args = list(),
  continue = FALSE, log.fun = "default", final.dw.perc = NULL, budget = NULL, mbo.keep.result = FALSE, mbo.design = NULL) {

  if (!is.null(learner)) {
    learner = checkLearner(learner, type = "regr")
    learner = setPredictType(learner, "se")
  }
  if (is.null(mbo.control)) {
    mbo.control = mlrMBO::makeMBOControl()
  }
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)
  assertFlag(mbo.keep.result)

  if (!is.null(budget) && !is.null(mbo.design) && nrow(mbo.design) > budget)
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      nrow(mbo.design), budget)
  else if (!is.null(budget)) {
    mbo.control = mlrMBO::setMBOControlTermination(mbo.control, max.evals = budget)
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
