# logging function for tuning and feature selection
# has 2 stages:
# 1: After point to eval is selected
# 2: after evaluation

logFunDefault = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x.string, y, remove.nas, stage, prev.stage, prefixes) {
  if (stage == 1L) {
    start.time = Sys.time()
    messagef("[%s] %i: %s", prefixes[stage], dob, x.string)
    return(list(start.time = start.time))
  } else if (stage == 2L) {
    end.time = Sys.time()
    diff.time = difftime(time1 = end.time, time2 = prev.stage$start.time, units = "mins")
    messagef("[%s] %i: %s; time: %.1f min",
      prefixes[stage], dob, perfsToString(y), diff.time[[1]])
    return(NULL)
  }
}

logFunMemory = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x.string, y, remove.nas, stage, prev.stage, prefixes) {
  if (stage == 1L) {
    gc()
    gc()
    gc()
    start.time = Sys.time()
    messagef("[%s] %i: %s", prefixes[stage], dob, x.string)
    return(list(start.time = start.time))
  } else if (stage == 2L) {
    # get cur mem and max mem and sum ncells and vcells.
    # we had a diff here with gc(reset=TRUE). but this is bad the user cant avoid this, it might even be
    # not allowed on CRAN
    mem = colSums(gc()[, c(2L, 6L)])
    end.time = Sys.time()
    diff.time = difftime(time1 = end.time, time2 = prev.stage$start.time, units = "mins")
    messagef("[%s] %i: %s; time: %.1f min; memory: %.0fMb use, %.0fMb max",
      prefixes[stage], dob, perfsToString(y), diff.time[[1]], mem[1L], mem[2L])
    return(NULL)
  }
}

logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {
  x.string = paramValueToString(par.set, x, show.missing.values = !remove.nas)
  # shorten tuning logging a bit. we remove the sel.learner prefix from params
  if (inherits(learner, "ModelMultiplexer")) {
    x.string = stri_replace_all(x.string, "", regex = stri_paste(x$selected.learner, "\\."))
  }

  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("Tune-x", "Tune-y")
  )
}

logFunTuneMemory = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {
  x.string = paramValueToString(par.set, x, show.missing.values = !remove.nas)
  # shorten tuning logging a bit. we remove the sel.learner prefix from params
  if (inherits(learner, "ModelMultiplexer")) {
    x.string = stri_replace_all(x.string, "", regex = stri_paste(x$selected.learner, "\\."))
  }

  logFunMemory(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("Tune-x", "Tune-y")
  )
}

logFunFeatSel = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {
  x.string = sprintf("%s (%i bits)", clipString(collapse(x, ""), 80L), sum(x))

  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("FeatSel-x", "FeatSel-y")
  )
}

logFunFeatSelMemory = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
  x, y, remove.nas, stage, prev.stage) {
  x.string = sprintf("%s (%i bits)", clipString(collapse(x, ""), 80L), sum(x))

  logFunMemory(learner, task, resampling, measures, par.set, control, opt.path, dob,
    x.string, y, remove.nas, stage, prev.stage, prefixes = c("FeatSel-x", "FeatSel-y")
  )
}
