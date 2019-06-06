#' @title Checks for correct strucutre of selected measures
#'
#' @param measures [`list`]\cr
#'   list of measures
#' @param obj [`data.frame`]\cr
#'   task
#' @param aggr [`function`]\cr
#'   aggregation function
#' @param na.rm [`logical(1)`]\cr
#'   Should `NA` values be removed? Default `FALSE`.
#'   This applies to all selected measures.
#' @keywords internal
#' @export

checkMeasures = function(measures, obj, aggr = NULL, na.rm = FALSE) {
  if (missing(measures)) {
    measures = list(getDefaultMeasure(obj))
  } else {
    measures = ensureVector(measures, n = 1L, cl = "Measure")
    for (i in seq_along(measures)) {
      measures[[i]]$aggr$na.rm = na.rm
    }
    assertList(measures, types = "Measure", min.len = 1L)
  }
  if (!is.null(aggr)) {
    measures = lapply(measures, setAggregation, aggr = aggr)
  }
  return(measures)
}
