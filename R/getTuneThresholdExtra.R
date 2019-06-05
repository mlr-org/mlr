# in case we have tune.threshold activated we want this as an extra. otherwise NULL
# @arg control
#   control [TuneControl]
#   res [result from evalOptimizationState]
getTuneThresholdExtra = function(control, res) {
  if (control$tune.threshold) {
    # add class names to threshold, if longer than 1
    extra = as.list(res$threshold)
    setNames(extra, stri_paste("threshold", ifelse(length(extra) > 1L, ".", ""), names(extra), ignore_null = TRUE))
  } else {
    NULL
  }
}
