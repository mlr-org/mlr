makeOptControl = function(same.resampling.instance, impute.val = NULL, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = "default", final.dw.perc = NULL, ...) {
  assertFlag(same.resampling.instance)
  if (!is.null(impute.val)) {
    assertNumeric(impute.val)
  }
  assertFunction(log.fun,
    args = c("learner", "task", "resampling", "measures", "par.set", "control", "opt.path", "dob", "x", "y", "remove.nas", "stage", "prev.stage"))
  assertFlag(tune.threshold)
  makeS3Obj("OptControl",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args,
    log.fun = log.fun,
    final.dw.perc = final.dw.perc,
    extra.args = list(...)
  )
}
