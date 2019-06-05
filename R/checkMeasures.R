checkMeasures = function(measures, obj, aggr = NULL) {
  if (missing(measures) || is.null(measures)) {
    measures = list(getDefaultMeasure(obj))
  } else {
    measures = ensureVector(measures, n = 1L, cl = "Measure")
    assertList(measures, types = "Measure", min.len = 1L)
  }
  if (!is.null(aggr)) {
    measures = lapply(measures, setAggregation, aggr = aggr)
  }
  return(measures)
}
