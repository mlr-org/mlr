#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlNSGA2 = function(same.resampling.instance = TRUE, impute.val = NULL, log.fun = NULL, ...) {
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val, log.fun = log.fun,
   ..., cl = "TuneMultiCritControlNSGA2")
}

