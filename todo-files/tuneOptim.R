tuneOptim = function(learner, task, resampling, measures, par.set, control, 
  opt.path, show.info) {
  
  low = getLower(par.set)
  upp = getUpper(par.set)
  start = convertStartToNumeric(control$start, par.set)
	ctrl.optim = control$extra.args
	method = ctrl.optim$method
	if(is.null(method)) 
		method = "Nelder-Mead"
	ctrl.optim$method = NULL
  cx = function(x) convertXNumeric(x, par.set)
  if (method == "L-BFGS-B") {
    or = optim(par=start, fn=tunerFitnFun, method=method, lower=low, upper=upp, control=ctrl.optim,
      learner=learner, task=task, resampling=resampling, measures=measures, 
      par.set=par.set, ctrl=control, opt.path=opt.path, show.info=show.info, 
      trafo=TRUE, convertx=cx, remove.nas=FALSE)    
  } else {
    # FIXME: fix machine bound
    if (any((is.double(low) & low != -Inf) | (is.integer(low) & low != -.Machine$integer.max)) ||
        any((is.double(upp) & upp !=  Inf) | (is.integer(upp) & upp !=  .Machine$integer.max))) 
      stop("Box constraints can only be used for 'L-BFGS-B' in 'optim'!")  
    or = optim(par=start, fn=tunerFitnFun, method=method, control=ctrl.optim,
      learner=learner, task=task, resampling=resampling, measures=measures, 
      par.set=par.set, ctrl=control, opt.path=opt.path, show.info=show.info, 
      trafo=TRUE, convertx=cx, remove.nas=FALSE)    
  }
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, e$x, e$y, NULL, opt.path)
}
