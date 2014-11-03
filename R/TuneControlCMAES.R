#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL, tune.threshold = FALSE, ...) {
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, tune.threshold = tune.threshold, ..., cl = "TuneControlCMAES")
}
