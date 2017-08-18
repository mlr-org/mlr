checkMeasures = function(measures, obj, aggr = NULL, na.rm) {
  if (missing(measures)) {
    measures = list(getDefaultMeasure(obj))
  } else {
    measures = ensureVector(measures, n = 1L, cl = "Measure")
    for (i in seq_along(measures)) {
      measures[[i]]$aggr$na.rm = na.rm
    }
    assertList(measures, types = "Measure", min.len = 1L)
  }
  if (!is.null(aggr))
    measures = lapply(measures, setAggregation, aggr = aggr)
  return(measures)
}
