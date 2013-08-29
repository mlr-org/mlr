#' @export
#' @rdname TuneControl
makeTuneControlGrid = function(same.resampling.instance=TRUE) {
  makeTuneControl(same.resampling.instance=same.resampling.instance, 
    start=list(), cl="TuneControlGrid")
}

