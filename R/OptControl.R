makeOptControl = function(same.resampling.instance, impute.val = NULL, tune.threshold = FALSE,
  tune.threshold.args = list(),log.fun = NULL, ...) {

  assertFlag(same.resampling.instance)
  if (!is.null(impute.val))
    assertNumber(impute.val)
  if (is.null(log.fun)) {
    log.fun = logFunTune
  } else {
    assertFunction(log.fun,
      args = c("learner", "task", "resampling", "measures", "par.set", "control", "opt.path", "dob", "x", "y", "remove.nas", "stage"))
  }
  assertFlag(tune.threshold)
  makeS3Obj("OptControl",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args,
    extra.args = list(...)
  )
}

logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas, stage = 0L) {
  if (stage == 1L) {
    if (!inherits(learner, "ModelMultiplexer")) {
      messagef("[Tune] %i: %s : %s", dob,
        paramValueToString(par.set, x, show.missing.values = !remove.nas), perfsToString(y))
    } else {
      # shorten tuning logging a bit. we remove the sel.learner prefix from params
      s = paramValueToString(par.set, x, show.missing.values = !remove.nas)
      s = gsub(paste0(x$selected.learner, "\\."), "", s)
      messagef("[Tune] %i: %s : %s", dob, s, perfsToString(y))
    }
  }
}
