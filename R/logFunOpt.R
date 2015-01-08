# logging function for tuning and feature selection
# has 2 stages:
# 1: After point to eval is selected
# 2: after evaluation

logFunDefault = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x.string, y, remove.nas, stage, prev.stage, prefixes) {

  if (stage == 1L) {
    gc(); gc(); gc()
    start.mem = sum(gc(reset = TRUE)[, 6L])
    start.time = Sys.time()
    messagef("[%s] %i: %s", prefixes[stage], dob, x.string)
    return(list(start.mem = start.mem, start.time = start.time))
  } else if (stage == 2L) {
    end.mem = sum(gc(reset = TRUE)[, 6L])
    end.time = Sys.time()
    diff.mem = end.mem - prev.stage$start.mem
    diff.time = end.time - prev.stage$start.time
    messagef("[%s] %i: %s; time: %.1f min; memory: %.1f Mb (+%.1f)",
      prefixes[stage], dob, perfsToString(y), diff.time, end.mem, diff.mem)
    return(NULL)
  }
}

logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {

  x.string = paramValueToString(par.set, x, show.missing.values = !remove.nas)
  # shorten tuning logging a bit. we remove the sel.learner prefix from params
  if (inherits(learner, "ModelMultiplexer"))
    x.string = gsub(paste0(x$selected.learner, "\\."), "", x.string)

  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("Tune-x", "Tune-y")
  )
}

logFunFeatSel = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {

  x.string = sprintf("%s (%i bits)", clipString(collapse(x, ""), 30L), sum(x))

  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("FeatSel-x", "FeatSel-y")
  )
}
