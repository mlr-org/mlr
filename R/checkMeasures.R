checkMeasures = function(measures, task, aggr = NULL) {
  if (missing(measures)) {
    measures = default.measures(task)
  } else {
    if (is(measures, "Measure")) {
      measures = list(measures)
    } else {
      checkArg(measures, "list")
      checkListElementClass(measures, "Measure")
    }
  }
  if (!is.null(aggr))
    measures = lapply(measures, setAggregation, aggr = aggr)
  return(measures)
}
