#FIXME we lose space here. x1 and x2 labs can be be put into one legend below plots
# FIXME we probably need a simple gui later to do some trafos like log on the the plots
# while the iters run
plotMBOExampleRun2DNumeric = function(x, iters, pause=TRUE, 
  design.pch=19, design.cols=c("black", "darkseagreen", "tomato"), densregion=TRUE, 
  se.factor1=1, se.factor2=2, ...)  {
  
  par(mfrow=c(2, 2), oma=c(0, 0, 2, 0))
  layout(matrix(1:9, ncol=3, byrow=TRUE), widths=c(1, 8, 8), heights=c(8, 8, 1))
  
  # do we plot stuff relatated to model uncertainty?
  se = x$learner$predict.type == "se"
  
  # extract information from example run object
  par.set = x$par.set
  names.x = x$names.x
  name.x1 = names.x[1]
  name.x2 = names.x[2]
  name.y = x$name.y
  evals = x$evals
  global.opt = x$global.opt
  ctrl = x$control
  proppoints = ctrl$propose.points
  mr = x$mbo.res
  col = terrain.colors(255)
  x1 = unique(evals[, name.x1])
  x2 = unique(evals[, name.x2])
  name.crit = ctrl$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
  op = as.data.frame(mr$opt.path)
  # ind.* are index sets into the opt.path (initial design and so on)
  ind.inides = which(op$dob == 0)
  
  plotfun1 = function(name.y, ind.inides, ind.seqdes, ind.prodes, log, main, xaxis, yaxis) {
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
    #FIXME do this before
    dim(y) = c(x$points.per.dim, x$points.per.dim)
    xaxt = ifelse(xaxis, "s", "n")
    xlab = ifelse(xaxis, names.x[1], "")
    yaxt = ifelse(yaxis, "s", "n")
    ylab = ifelse(yaxis, names.x[2], "")
    image(x=x1, y=x2, z=y, col=col, main="", asp=1, useRaster=TRUE,
      xaxt="n", yaxt="n", xlab="", ylab="")     
    contour(x=x1, y=x2, z=y, asp=1, add=TRUE)
    # plot design points
    points(op[ind.inides, name.x1], op[ind.inides, name.x2], pch=design.pch, col=design.cols[[1]])
    points(op[ind.seqdes, name.x1], op[ind.seqdes, name.x2], pch=design.pch, col=design.cols[[2]])
    points(op[ind.prodes, name.x1], op[ind.prodes, name.x2], pch=design.pch, col=design.cols[[3]])
  }
  
  plotblank = function() {
    plot(x1, x2, type="n", axes=FALSE, xlab="", ylab="")
  }
  
  myaxis = function(side, label) {
    #par(mar=c(3, 3, 3, 3))
    plotblank()
    axis(side=side)
  }
  
  
  # FIXME add option to log
  #FIXME add se option whether it is plotted
  
  #FIXME what to plot if not infillcrit that uses se?
  #FIXME how do we display noise? do we at all?
  
  for (i in iters) {
    mod = mr$models[[i]]
    
    ind.seqdes = which(op$dob > 0 & op$dob < i)
    ind.prodes = which(op$dob == i)
    ind.pasdes = c(ind.inides, ind.seqdes)
    ind.all = c(ind.inides, ind.seqdes, ind.prodes)
    
    evals[["yhat"]] = mlrMBO:::infillCritMeanResponse(evals[, names.x, drop=FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    if (se) 
      evals[["se"]] = -mlrMBO:::infillCritStandardError(evals[, names.x, drop=FALSE], 
        mod, ctrl, par.set, op[ind.pasdes, ])
    if (proppoints == 1L) {
      evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop=FALSE], 
        mod, ctrl, par.set, op[ind.pasdes, ])
    } else {          
      # just display the first lcb crit fun
      ctrl2 = ctrl
      ctrl2$infill.crit.lcb.lambda = mr$multipoint.lcb.lambdas[i, 1]
      evals[[name.crit]] = opt.direction * infillCritLCB(evals[, names.x, drop=FALSE], 
        mod, ctrl, par.set, op[ind.pasdes, ])
    }                                               
    #FIXME expose logariuthm taking
    par(mar=c(0.1, 2.1, 1.5, 0.1))    
    myaxis(2)
    # plot true y
    par(mar=c(0.1, 0.1, 0.1, 0.1))    
    plotfun1(name.y, ind.inides, ind.seqdes, ind.prodes, log=TRUE, 
      main=sprintf("%s / %s", name.y, "se"), xaxis=FALSE, yaxis=TRUE) 
    
     par(mar=c(0.1, 0.1, 0.1, 0.1))    
     plotfun1("yhat", ind.inides, ind.seqdes, ind.prodes, log=FALSE, 
       main=sprintf("%s / %s", name.y, "se"), xaxis=FALSE, yaxis=FALSE)
    if (se) {
      par(mar=c(0.1, 2.1, 1.5, 0.1))    
      myaxis(2, names.x[2])
      par(mar=c(0.1, 0.1, 0.1, 0.1))    
      plotfun1("se", ind.inides, ind.seqdes, ind.prodes, log=FALSE, 
        main="", xaxis=TRUE, yaxis=TRUE) 
    } else { # FIXME do what here?
      plot(1,1)
    }
    par(mar=c(0.1, 0.1, 0.1, 0.1))    
    plotfun1(name.crit, ind.inides, ind.seqdes, ind.prodes,
      main="", log=FALSE, xaxis=TRUE, yaxis=FALSE) 
    plotblank(); 
    par(mar=c(2.1, 0.1, 0.1, 0.1))    
    myaxis(1); 
    par(mar=c(2.1, 0.1, 0.1, 0.1))    
    myaxis(1)
    gap = calculateGap(op[ind.all,], global.opt, ctrl)
    title(outer=TRUE, sprintf("iter = %i; x1=%s;  x2=%s;  top = %s / %s;  bottom = %s / %s;  gap = %.4e",
      i, names.x[1], names.x[2], name.y, "yhat", "se", name.crit, gap))
    if (pause)
      pause()
   }
}