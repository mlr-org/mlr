#' Creates a measure for non-standard misclassification costs.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure. Default is \dQuote{costs}.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param costs [\code{matrix}]\cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter.
#'   Rows indicate true classes, columns predicted classes.
#' @param task [\code{\link{ClassifTask}}]\cr
#'   Classification task. Has to be passed, so validity of matrix names can be checked.
#' @param aggregate [\code{logical(1)}]\cr
#'   How to aggregate costs over all cases in one test set prediction?
#'   Default is \code{\link{mean}}.
#' @return [\code{\link{Measure}}].
#' @export
#' @seealso \code{\link{measures}}, \code{\link{makeMeasure}}
makeCostMeasure = function(id = "costs", minimize = TRUE, costs, task, aggregate = mean) {
  assertString(id)
  assertFlag(minimize)
  assertMatrix(costs)
  assertClass(task, classes = "ClassifTask")
  assertFunction(aggregate)

  #check costs
  levs = task$task.desc$class.levels
  if (any(dim(costs))) {
    if (any(dim(costs) != length(levs)))
      stop("Dimensions of costs have to be the same as number of class levels!")
    rns = rownames(costs)
    cns = colnames(costs)
    if (!setequal(rns, levs) || !setequal(cns, levs))
      stop("Row and column names of cost matrix have to equal class levels!")
  }

  makeMeasure(id = "costs", minimize = minimize, extra.args = list(costs, aggregate),
    properties = c("classif", "classif.multi"),
    allowed.pred.types = c("response", "prob"),
    fun = function(task, model, pred, extra.args) {
      costs = extra.args[[1L]]
      # cannot index with NA
      r = pred$data$response
      if (any(is.na(r)))
        return(NA_real_)
      cc = function(truth, pred) {
        costs[truth, pred]
      }
      y = mapply(cc, as.character(pred$data$truth), as.character(r))
      aggregate(y)
    }
  )
}
