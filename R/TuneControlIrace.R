#' @param n.instances [\code{integer(1)}]\cr
#'   Number of random resampling instances for irace, see details.
#'   Default is 100.
#' @param show.irace.output [\code{logical(1)}]\cr
#'   Show console output of irace while tuning?
#'   Default is \code{FALSE}.
#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(impute.val = Inf, n.instances = 100L, show.irace.output = FALSE, ...) {
  makeTuneControl(same.resampling.instance = FALSE, impute.val = impute.val,
    n.instances = n.instances, show.irace.output = show.irace.output,
   start = NULL, ..., cl = "TuneControlIrace")
}
