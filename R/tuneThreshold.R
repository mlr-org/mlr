#' Tune prediction threshold.
#'
#' Optimizes the threshold of prediction based on probabilities.
#' Uses \code{\link[BBmisc]{optimizeSubInts}} for 2class problems and \code{\link[cmaes]{cma_es}}
#' for multiclass problems.
#'
#' @template arg_pred
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure to optimize.
#' @param task [\code{\link{Task}}]\cr
#'   Learning task. Rarely neeeded,
#'   only when required for the performance measure.
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Fitted model. Rarely neeeded,
#'   only when required for the performance measure.
#' @param nsub [\code{integer(1)}]\cr
#'   Passed to \code{\link[BBmisc]{optimizeSubInts}} for 2class problems.
#'   Default is 20.
#' @param control [\code{list}]\cr
#'   Control object for \code{\link[cmaes]{cma_es}} when used.
#'   Default is empty list.
#' @return [\code{list}]. A named list with with the following components:
#'   \code{th} is the optimal threshold, \code{perf} the performance value.
#' @family tune
#' @export
tuneThreshold = function(pred, measure, task, model, nsub = 20L, control = list()) {
  assertClass(pred, classes = "Prediction")
  assertClass(measure, classes = "Measure")
  if (!missing(task))
    assertClass(task, classes = "SupervisedTask")
  if (!missing(model))
    assertClass(model, classes = "WrappedModel")
  assertList(control)

  td = pred$task.desc
  if (missing(measure))
    measure = default.measures(td)[[1]]
  probs = getProbabilities(pred)

  # brutally return NA if we find any NA in the predicted probs...
  if (anyMissing(probs)) {
    return(list(th = NA, pred = pred, th.seq = numeric(0), perf = numeric(0)))
  }

  cls = pred$task.desc$class.levels
  k = length(cls)
  fitn = function(x) {
    if (k > 2)
      names(x) = cls
    performance(setThreshold(pred, x), measure, task, model)
  }

  if (k == 2) {
    or = optimizeSubInts(f = fitn, lower = 0, upper = 1, maximum = !measure$minimize, nsub = nsub)
    th = or[[1]]
    perf = or$objective
  } else {
    requirePackages("cmaes", why = "tuneThreshold", default.method = "load")
    start = rep(0.5, k)
    or = cmaes::cma_es(par = start, fn = fitn, lower = 0, upper = 1, control = control)
    th = or$par / sum(or$par)
    names(th) = cls
    perf = or$val
  }
  return(list(th = th, perf = perf))
}
