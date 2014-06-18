checkMeasures = function(measures, obj, aggr = NULL) {
  if (missing(measures)) {
    measures = default.measures(obj)
  } else {
    measures = ensureVector(measures, n = 1L, cl = "Measure")
    checkArg(measures, "list")
    checkListElementClass(measures, "Measure")
  }
  if (!is.null(aggr))
    measures = lapply(measures, setAggregation, aggr = aggr)
  return(measures)
}
