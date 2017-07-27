#' @title Set parameters of performance measures
#'
#' @description
#' Sets hyperparameters of measures.
#'
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure.
#' @param ... [any]\cr
#'   Named (hyper)parameters with new settings. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameter settings. The arguments in
#'   \code{...} take precedence over values in this list.
#' @template ret_measure
#' @family performance
#' @export
setMeasurePars = function(measure, ..., par.vals = list()) {
  args = list(...)
  assertClass(measure, classes = "Measure")
  assertList(args, names = "unique", .var.name = "parameter settings")
  assertList(par.vals, names = "unique", .var.name = "parameter settings")
  measure$extra.args = insert(measure$extra.args, insert(par.vals, args))
  measure
}

#' @title Set aggregation function of measure.
#'
#' @description
#' Set how this measure will be aggregated after resampling.
#' To see possible aggregation functions: \code{\link{aggregations}}.
#'
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure.
#' @template arg_aggr
#' @return [\code{\link{Measure}}] with changed aggregation behaviour.
#' @family performance
#' @export
setAggregation = function(measure, aggr) {
  assertClass(measure, classes = "Measure")
  assertClass(aggr, classes = "Aggregation")
  measure$aggr = aggr
  return(measure)
}
