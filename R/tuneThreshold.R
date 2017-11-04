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
    performance(setThreshold(pred, x), measure, task, model, simpleaggr = TRUE)
  }
  
  if (ttype == "multilabel" || k > 2L) {
    if (is.null(control$restarts))
      restarts = 0L
    else
      restarts = control$restarts
    max.call = 5000L
    
    requirePackages("GenSA", why = "tuneThreshold", default.method = "load")
    start = rep(1 / k, k)
    
    #create restart points
    if (restarts != 0L) {
      max.call = max.call / (1L + restarts)
      ctrl = list(smooth = FALSE, max.call = max.call, temperature = 5000L)
      for (i in 1:restarts) {
        u = runif(k)
        start = rbind(start, u / sum(u))
      }
      
      or = apply(start, MARGIN = 1L, FUN = function (s) {
        GenSA(par = s, fn = fitn, lower = rep(0, k),
          upper = rep(1, k), control = ctrl)
      })

      #take best result
      sl = extractSubList(or, "value")
      if (measure$minimize == TRUE)
        opt = which(sl == min(sl))
      else
        opt = which(sl == max(sl))
      or = or[[opt[1]]]
    } else {
      ctrl = list(smooth = FALSE, max.call = max.call)
      or = GenSA(par = start, fn = fitn, lower = rep(0, k),
          upper = rep(1, k), control = ctrl)
    }
    th = or$par / sum(or$par)
    names(th) = cls
    perf = or$value
  } else {# classif with k = 2
    or = optimizeSubInts(f = fitn, lower = 0, upper = 1, maximum = !measure$minimize, nsub = nsub)
    th = or[[1]]
    perf = or$objective
  }
  return(list(th = th, perf = perf))
}
