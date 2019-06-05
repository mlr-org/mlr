#' @title Creates a measure for non-standard misclassification costs.
#'
#' @description
#' Creates a cost measure for non-standard classification error costs.
#'
#' @param id (`character(1)`)\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param minimize (`logical(1)`)\cr
#'   Should the measure be minimized?
#'   Otherwise you are effectively specifying a benefits matrix.
#'   Default is `TRUE`.
#' @param costs ([matrix])\cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter.
#'   Rows indicate true classes, columns predicted classes.
#' @param combine (`function`)\cr
#'   How to combine costs over all cases for a SINGLE test set?
#'   Note this is not the same as the `aggregate` argument in [makeMeasure]
#'   You can set this as well via [setAggregation], as for any measure.
#'   Default is [mean].
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance
makeCostMeasure = function(id = "costs", minimize = TRUE, costs, combine = mean, best = NULL, worst = NULL,
  name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertMatrix(costs)
  assertFunction(combine)
  assertString(name)
  assertString(note)


  makeMeasure(id = id, minimize = minimize, extra.args = list(costs = costs, combine = combine),
    properties = c("classif", "classif.multi", "req.pred", "req.truth", "predtype.response", "predtype.prob"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {

      # check costs
      td = pred$task.desc
      levs = td$class.levels
      if (any(dim(costs))) {
        if (any(dim(costs) != length(levs))) {
          stop("Dimensions of costs have to be the same as number of class levels!")
        }
        rns = rownames(costs)
        cns = colnames(costs)
        if (!setequal(rns, levs) || !setequal(cns, levs)) {
          stop("Row and column names of cost matrix have to equal class levels!")
        }
      }
      costs = extra.args[[1L]]
      # cannot index with NA
      r = pred$data$response
      if (anyMissing(r)) {
        return(NA_real_)
      }
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
