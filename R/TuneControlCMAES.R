#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance=TRUE, start, ...) {
  makeTuneControl(same.resampling.instance=same.resampling.instance, 
    start=as.list(start), ..., cl="TuneControlCMAES")
}

