#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, budget = 100L,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL) {

  makeTuneControl(same.resampling.instance = same.resampling.instance,
    budget = budget, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, cl = "TuneControlRandom")
}
