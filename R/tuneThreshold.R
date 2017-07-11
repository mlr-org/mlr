#' @title Tune prediction threshold.
#'
#' @description
#' Optimizes the threshold of predictions based on probabilities.
#' Works for classification and multilabel tasks.
#' Uses \code{\link[BBmisc]{optimizeSubInts}} for normal binary class problems and
#' \code{\link[cmaes]{cma_es}} for multiclass and multilabel problems.
#'
#' @template arg_pred
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure to optimize.
#'   Default is the default measure for the task.
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
    if (is.null(control$restarts))
      restarts = 1L
    else
      restarts = control$restarts
    # requirePackages("cmaes", why = "tuneThreshold", default.method = "load")
    start = rep(1 / k, k)

    #create restart points
    for (i in 1:restarts) {
      u = runif(k)
      start = rbind(start, u / sum(u))
    }
    #create constraint matrix
    or = list()
    ui = rbind(diag(k), -diag(k), rep(1, k), rep(-1, k))
    ci = c(rep(c(0, -1), each = k), 0.999, -1.001)
    for (i in 1:(restarts + 1)) {
      s = start[i, ]
      or[[i]] = constrOptim(s, fitn, method = "Nelder-Mead", ui = ui, ci = ci)
    }
    #take best result
    sl = extractSubList(or, "value")
    if (measure$minimize == TRUE)
      opt = which(sl == min(sl))
    else
      opt = which(sl == max(sl))
    or = or[[opt[1]]]
    th = or$par / sum(or$par)
    names(th) = cls
    perf = or$val
  } else {# classif with k = 2
    or = optimizeSubInts(f = fitn, lower = 0, upper = 1, maximum = !measure$minimize, nsub = nsub)
    th = or[[1]]
    perf = or$objective
  }
  return(list(th = th, perf = perf))
}
