#' @export
#' @rdname TuneControl
makeTuneControlGenSA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, ...) {

  args = list(...)
  default = list(smooth = FALSE)
  default = list()
  args = insert(default, args)
  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, cl = "TuneControlGenSA")
  do.call(makeTuneControl, c(args, args2))
}

