#' @title Create control object for hyperparameter tuning with MBO.
#'
#' @description
#' Model-based / Bayesian optimization with the function
#' [mlrMBO::mbo] from the \pkg{mlrMBO} package.
#' Please refer to <https://github.com/mlr-org/mlrMBO> for further info.
#'
#' @inherit TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function evaluations.
#' @param learner ([Learner] | `NULL`)\cr
#'   The surrogate learner: A regression learner to model performance landscape.
#'  For the default, `NULL`, \pkg{mlrMBO} will automatically create a suitable learner based on the rules described in [mlrMBO::makeMBOLearner].
#' @param continue (`logical(1)`)\cr
#'   Resume calculation from previous run using [mlrMBO::mboContinue]?
#'   Requires \dQuote{save.file.path} to be set.
#'   Note that the [ParamHelpers::OptPath] in the [mlrMBO::OptResult]
#'   will only include the evaluations after the continuation.
#'   The complete [OptPath] will be found in the slot `$mbo.result$opt.path`.
#' @param mbo.control ([mlrMBO::MBOControl] | `NULL`)\cr
#'   Control object for model-based optimization tuning.
#'   For the default, `NULL`, the control object will be created with all the defaults as described in [mlrMBO::makeMBOControl].
#' @param mbo.design ([data.frame] | `NULL`)\cr
#'   Initial design as data frame.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   For the default, `NULL`, a default design is created like described in [mlrMBO::mbo].
#' @return ([TuneControlMBO])
#' @references Bernd Bischl, Jakob Richter, Jakob Bossek, Daniel Horn, Janek Thomas and Michel Lang; mlrMBO: A Modular Framework for Model-Based Optimization of Expensive Black-Box Functions, Preprint: <https://arxiv.org/abs/1703.03373> (2017).
#' @aliases TuneControlMBO
#' @family tune
#' @export
makeTuneControlMBO = function(same.resampling.instance = TRUE, impute.val = NULL,
  learner = NULL, mbo.control = NULL, tune.threshold = FALSE, tune.threshold.args = list(),
  continue = FALSE, log.fun = "default", final.dw.perc = NULL, budget = NULL, mbo.design = NULL) {

  if (!is.null(learner)) {
    learner = checkLearner(learner, type = "regr")
    learner = setPredictType(learner, "se")
  }
  if (is.null(mbo.control)) {
    mbo.control = mlrMBO::makeMBOControl()
  }
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)

  if (!is.null(budget) && !is.null(mbo.design) && nrow(mbo.design) > budget) {
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      nrow(mbo.design), budget)
  } else if (!is.null(budget)) {
    mbo.control = mlrMBO::setMBOControlTermination(mbo.control, max.evals = budget)
  }

  x = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    cl = "TuneControlMBO", log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  x$mbo.design = mbo.design
  return(x)
}
