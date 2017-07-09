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
    requirePackages("Rsolnp", why = "tuneThreshold", default.method = "load")
    start = rep(1/k, k)

    # ui = rbind(diag(k), -diag(k), rep(1, k), rep(-1, k))
    # ci = c(rep(c(0, -1), each = k), 0.999, -1.001)
    ui = rbind(diag(k), -diag(k))
    ci = c(rep(c(0, -1), each = k))
    s = start
    or = constrOptim(s, fitn, method = "Nelder-Mead", ui = ui, ci = ci)
    # possible alternative with equalities and unequalities - minimises by default
    # or = solnp(start, #starting values
    #   fitn, #function to optimise
    #   eqfun = sum, #equality function
    #   eqB = 1,   #the equality constraint
    #   ineqLB = rep(0, k), #lower bound for parameters i.e. greater than zero
    #   ineqUB = rep(1, k)) #upper bound for parameters
    if (is.null(control$max_restarts))
      max_restarts = 50L
    else
      max_restarts = control$max_restarts

    for (i in 1:max_restarts) {
      restart = runif(k)
      restart = restart / sum(restart)
      # for (i in 1:max_restarts) {
      rs = constrOptim(restart, fitn, method = "Nelder-Mead", ui = ui, ci = ci)
      if (rs$val < or$val)
        or = rs
    }
    xx <<- or
    th = or$par
    names(th) = cls
    perf = or$val
  } else {# classif with k = 2
    or = optimizeSubInts(f = fitn, lower = 0, upper = 1, maximum = !measure$minimize, nsub = nsub)
    th = or[[1]]
    perf = or$objective
  }
  return(list(th = th, perf = perf))
}
