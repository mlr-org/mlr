# check whether rdesc$predict is set, so that the requiring properties of the measure are satisfied
# called the beginning of resample
checkAggrBeforeResample = function(measure, rdesc) {
  a = measure$aggr
  p = a$properties
  pred = rdesc$predict
  p.allowed = if (all(c("req.train", "req.test") %in% p)) {
    "both"
  } else if ("req.train" %in% p) {
    c("train", "both")
  } else if ("req.test" %in% p) {
    c("test", "both")
  } else {
    c("train", "test", "both")
  }
  if (pred %nin% p.allowed) {
    stopf("Aggregation '%s' not compatible with resampling! You have to set arg 'predict' to %s in your resample object, instead it is '%s'!", a$id, stri_paste("'", p.allowed, "'", collapse = " or "), pred)
  }
}

# map the checker over multiple measures
checkAggrsBeforeResample = function(measures, rdesc) {
  lapply(measures, checkAggrBeforeResample, rdesc = rdesc)
}
