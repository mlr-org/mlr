#tune.mies = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
#  requirePackages("mies", "tune.mies")
#  
#  g = makeTunerTargetFun(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
#    arg.as.list=TRUE, trafo=TRUE)
#  
#  mies.ctrl = do.call("makeMiesControl", control@extra.args)
#  or = mies(fitn=g, par.set=par.set, control=mies.ctrl)
#  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
#  e = getOptPathEl(opt.path, i)
#  new("OptResult", learner, control, e$x, e$y, opt.path)
#}
