checkMeasures = function(measures, obj, aggr = NULL) {
  if (missing(measures)) {
    measures = list(getDefaultMeasure(obj))
  } else {
    measures = ensureVector(measures, n = 1L, cl = "Measure")
    if (!any(names(measures) == "na.rm")) {
      measures$na.rm = FALSE
    }
    na.rm = measures$na.rm
    measures$na.rm = NULL
    for (i in seq_along(measures)) {
      measures[[i]]$aggr$na.rm = na.rm
    }
    assertList(measures, types = "Measure", min.len = 1L)
  }
  if (!is.null(aggr))
    measures = lapply(measures, setAggregation, aggr = aggr)
  return(measures)
}
