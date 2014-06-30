#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance = TRUE, impute.val = Inf, start = NULL, ...) {
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, ..., cl = "TuneControlCMAES")
}
