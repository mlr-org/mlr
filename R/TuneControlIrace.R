#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(n.instances = 100L, show.irace.output = FALSE, ...) {
  makeTuneControl(same.resampling.instance=FALSE, n.instances = n.instances, show.irace.output = show.irace.output,
                  start=list(), ..., cl="TuneControlIrace")
}
