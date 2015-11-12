#' @title Creates a measure for non-standard misclassification costs.
#'
#' @description
#' Creates a cost measure for non-standard classification error costs.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Otherwise you are effectively specifying a benefits matrix.
#'   Default is \code{TRUE}.
#' @param costs [\code{matrix}]\cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter.
#'   Rows indicate true classes, columns predicted classes.
#' @param task [\code{\link{ClassifTask}}]\cr
#'   Classification task. Has to be passed, so validity of matrix names can be checked.
#' @param combine [\code{function}]\cr
#'   How to combine costs over all cases for a SINGLE test set?
#'   Note this is not the same as the \code{aggregate} argument in \code{\link{makeMeasure}}
#'   You can set this as well via \code{\link{setAggregation}}, as for any measure.
#'   Default is \code{\link{mean}}.
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance
makeCostMeasure = function(id = "costs", minimize = TRUE, costs, task, combine = mean, best = NULL, worst = NULL,
                           name = id, note = "") {
  assertString(id)
  assertFlag(minimize)
  assertMatrix(costs)
  assertClass(task, classes = "ClassifTask")
  assertFunction(combine)
  assertString(name)
  assertString(note)

  #check costs
  td = getTaskDescription(task)
  levs = td$class.levels
  if (any(dim(costs))) {
    if (any(dim(costs) != length(levs)))
      stop("Dimensions of costs have to be the same as number of class levels!")
    rns = rownames(costs)
    cns = colnames(costs)
    if (!setequal(rns, levs) || !setequal(cns, levs))
      stop("Row and column names of cost matrix have to equal class levels!")
  }

  makeMeasure(id = id, minimize = minimize, extra.args = list(costs, combine),
    properties = c("classif", "classif.multi", "req.pred", "req.truth", "predtype.response", "predtype.prob"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      costs = extra.args[[1L]]
      # cannot index with NA
      r = pred$data$response
      if (anyMissing(r))
        return(NA_real_)
      cc = function(truth, pred) {
        costs[truth, pred]
      }
      y = mapply(cc, as.character(pred$data$truth), as.character(r))
      combine(y)
    },
    name = name,
    note = note
  )
}
