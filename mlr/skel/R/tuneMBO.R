#FIXME: should we really define validation error like this?
tuneMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info) {

  requirePackages(c("mlrMBO"), "tuneMBO")
  mbo.control = control$mbo.control
  # set final evals to 0 to save time. we dont really need final evals in this context.
  mbo.control$final.evals = 0L
  cx = identity
  
  # FIXME: use ... in mbo
  tff = function(x) tunerFitnFun(x, learner=learner, task=task, resampling=resampling, measures=measures, 
    par.set=par.set, ctrl=control, opt.path=opt.path, show.info=show.info, 
    trafo=FALSE, convertx=cx, remove.nas=FALSE)    
  
  #FIXME remove this when mbo on cran
  mbofun = get("mbo", envir=getNamespace("mlrMBO"))

  or = mbofun(tff, par.set, design=NULL, learner=control$learner, control=mbo.control, show.info=FALSE)
  
  # FIXME: check this this is really ok, that we dont trafo in mlrMBO
  x = trafoValue(par.set, or$x)
  makeTuneResult(learner, control, x, or$y, opt.path)
}
