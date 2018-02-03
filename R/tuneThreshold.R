#' @title Tune prediction threshold.
#'
#' @description
#' Optimizes the threshold of predictions based on probabilities.
#' Works for classification and multilabel tasks.
#' Uses [BBmisc::optimizeSubInts] for normal binary class problems and [cmaes::cma_es]
#' for multiclass and multilabel problems.
#'
#' @template arg_pred
#' @param measure ([Measure])\cr
#'   Performance measure to optimize.
#'   Default is the default measure for the task.
#' @param task ([Task])\cr
#'   Learning task. Rarely neeeded,
#'   only when required for the performance measure.
#' @param model ([WrappedModel])\cr
#'   Fitted model. Rarely neeeded,
#'   only when required for the performance measure.
#' @param nsub (`integer(1)`)\cr
#'   Passed to [BBmisc::optimizeSubInts] for 2class problems.
#'   Default is 20.
#' @param control ([list])\cr
#'   Control object for [cmaes::cma_es] when used.
#'   Default is empty list.
#' @return ([list]). A named list with with the following components:
#'   `th` is the optimal threshold, `perf` the performance value.
#' @family tune
#' @export
tuneThreshold = function(pred, measure, task, model, nsub = 20L, control = list()) {
  checkPrediction(pred, task.type = c("classif", "multilabel"), predict.type = "prob")
  td = pred$task.desc
  ttype = td$type
  measure = checkMeasures(measure, td)[[1L]]
  if (!missing(task))
    assertClass(task, classes = "SupervisedTask")
  if (!missing(model))
    assertClass(model, classes = "WrappedModel")
  assertList(control)

  probs = getPredictionProbabilities(pred)

  # brutally return NA if we find any NA in the predicted probs...
  if (anyMissing(probs)) {
    return(list(th = NA, pred = pred, th.seq = numeric(0), perf = numeric(0)))
  }

  cls = pred$task.desc$class.levels
  k = length(cls)
  fitn = function(x) {
    if (ttype == "multilabel" || k > 2)
      names(x) = cls
    performance(setThreshold(pred, x), measure, task, model)
  }

  if (ttype == "multilabel" || k > 2L) {
    requirePackages("cmaes", why = "tuneThreshold", default.method = "load")
    start = rep(0.5, k)
    or = cmaes::cma_es(par = start, fn = fitn, lower = 0, upper = 1, control = control)
    th = or$par / sum(or$par)
    names(th) = cls
    perf = or$val
  } else { # classif with k = 2
    or = optimizeSubInts(f = fitn, lower = 0, upper = 1, maximum = !measure$minimize, nsub = nsub)
    th = or[[1]]
    perf = or$objective
  }
  return(list(th = th, perf = perf))
}
