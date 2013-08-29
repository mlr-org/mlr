#FIXME we lose space here. x1 and x2 labs can be be put into one legend below plots
# FIXME we probably need a simple gui later to do some trafos like log on the the plots
# while the iters run
plotMBOExampleRun2DNumeric = function(obj, ..., pch, col.initdes, col.seqdes, col.propdes) {
  
  plotfun1 = function(name.y, ind.inides, ind.seqdes, ind.prodes, log) {
    y = evals[, name.y]
    if (log) {
      if (any(y < 0)) {
        #FIXME what abou this?
        warning("Negative function values. Shifting function to apply logarithm.")
        y = y - min(y) + 1
      }
      y = log(y)
    }
    #FIXME expose this?
    col = terrain.colors(255)
    x1 = unique(evals[, name.x1])
    x2 = unique(evals[, name.x2])
    #FIXME do this before
    dim(y) = c(obj$points.per.dim, obj$points.per.dim)
    image(x=x1, y=x2, z=y, col=col,
      xlab=names.x[1], ylab=names.x[2], main=name.y, asp=1, useRaster=TRUE)
    contour(x=x1, y=x2, z=y, asp=1, add=TRUE)
    # plot design points
    points(op[ind.inides, name.x1], op[ind.inides, name.x2], pch=pch, col=col.initdes)
    points(op[ind.seqdes, name.x1], op[ind.seqdes, name.x2], pch=pch, col=col.seqdes)
    points(op[ind.prodes, name.x1], op[ind.prodes, name.x2], pch=pch, col=col.propdes)
  }
  
  par(mfrow=c(2, 2))
  # do we plot stuff relatated to model uncertainty?
  se = obj$learner$predict.type == "se"
  
  # extract information from example run object
  par.set = obj$par.set
  names.x = obj$names.x
  name.x1 = names.x[1]
  name.x2 = names.x[2]
  name.y = obj$name.y
  evals = obj$evals
  iters = obj$control$iters
  name.crit = obj$control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
  op = as.data.frame(obj$mbo.res$opt.path)
  # ind.* are index sets into the opt.path (initial design and so on)
  ind.inides = which(op$dob == 0)
  
  # FIXME add option to log
  #FIXME add se option whether it is plotted
  
  for (i in seq_len(iters)) {
    mod = obj$mbo.res$models[[i]]
    
    ind.seqdes = which(op$dob > 0 & op$dob < i)
    ind.prodes = which(op$dob == i)
    ind.pasdes = c(ind.inides, ind.seqdes)
    
    evals[["yhat"]] = mlrMBO:::infillCritMeanResponse(evals[, names.x, drop=FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    if (se) 
      evals[["se"]] = -mlrMBO:::infillCritStandardError(evals[, names.x, drop=FALSE], 
        mod, ctrl, par.set, op[ind.pasdes, ])
    evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop=FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    
    plotfun1(name.y, ind.inides, ind.seqdes, ind.prodes, log=TRUE) 
    plotfun1("yhat", ind.inides, ind.seqdes, ind.prodes, log=TRUE) 
    if (se)
      plotfun1("se", ind.inides, ind.seqdes, ind.prodes, log=FALSE) 
    else # FIXME do what here?
      plot(1,1)
    plotfun1(name.crit, ind.inides, ind.seqdes, ind.prodes, log=FALSE) 
    pause()
  }
}