#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlNSGA2 = function(same.resampling.instance = TRUE, impute.val = Inf, ...) {
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
   ..., cl = "TuneMultiCritControlNSGA2")
}

