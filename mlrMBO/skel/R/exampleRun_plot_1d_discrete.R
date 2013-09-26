plotMBOExampleRun1DDiscrete = function(x, iters, pause=TRUE, 
  design.pch=c(19,18,17), design.cols=c("black", "darkseagreen", "tomato"), xlim, ylim, ...)  {

  # FIXME: A LOT OF COPY AND PASTE HERE (make exampleRun_plot_1d.R and handle numeric and discrete cases)
  
  cex.points = 1.5
  cex.axis = 1.5
  cex.lab = 1.5
  cex.legend = 1.5
  lwd.lines = 2.5

  plotDesignPoints = function(op, ind.inides, ind.seqdes, ind.prodes, y) {
    points(op[ind.inides, name.x], op[ind.inides, y], pch=design.pch[1], col=design.cols[[1]], cex=cex.points)
    points(op[ind.seqdes, name.x], op[ind.seqdes, y], pch=design.pch[2], col=design.cols[[2]], cex=cex.points)
    points(op[ind.prodes, name.x], op[ind.prodes, y], pch=design.pch[3], col=design.cols[[3]], cex=cex.points)
  }
  
  # do we plot stuff relatated to model uncertainty?
  se = x$learner$predict.type == "se"

  # extract information from example run object
  par.set = x$par.set
  discrete.values = par.set$pars[[1]]$values
  name.x = x$names.x
  name.y = x$name.y
  ctrl = x$control
  minimize = ctrl$minimize 
  evals = x$evals
  evals.x = evals[, name.x, drop=FALSE]
  global.opt = x$global.opt
  xseq = evals[, name.x]
  proppoints = ctrl$propose.points
  mr = x$mbo.res
  name.crit = ctrl$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  
  op = as.data.frame(mr$opt.path)
  # ind.* are index sets into the opt.path (initial design and so on)
  ind.inides = which(op$dob == 0)
  
  for (i in iters) {
    mod = mr$models[[i]]
    
    ind.seqdes = which(op$dob > 0 & op$dob < i)
    ind.prodes = which(op$dob == i)
    ind.pasdes = c(ind.inides, ind.seqdes)
    ind.all = c(ind.inides, ind.seqdes, ind.prodes)
    
    model.ok = !inherits(mod, "FailureModel")
    
    # compute model prediction for current iter
    if (model.ok) {
      evals$yhat = mlrMBO:::infillCritMeanResponse(evals.x, 
        mod, ctrl, par.set, op[ind.pasdes, ])
    }    
    # define layout, i.e., the space available and the order of the plots 
    layout(matrix(c(1, 2, 3), ncol=1, byrow=TRUE), heights=c(0.5, 5.5))

    # 1st plots, 2 descriptive legends
    par(mai=c(0, 0.8, 0, 0.1))
    # FIXME: rescaling window results in overlapping of legend text
    main = sprintf("Iter = %i, Gap = %.4e", i, 
      calculateGap(op[ind.all,], global.opt, ctrl))
    plot.new()
    legend(x="left", legend=main, cex=cex.legend)

    par(mai=c(0.1, 0.8, 0, 0.1))

    # FIXME: ggplot works fine! How to make a simliar plot with standard low level R plot functions
    #        in a similar straight forward way??!?
    #df = op[ind.inides,]
    #pl = ggplot(df, aes(x=foo, y=y)) + geom_point()
    #print(pl)

    # FIXME: surely there is an easier way to do this with R's build-in plot functions
    # FIXME: add type of "whsikers"
    encodeFactor = function(levels, df, name.x) {
      xs = 1:length(levels)
      modified.df = df
      for (i in 1:nrow(df)) {
        modified.df[i, name.x] = xs[which(levels == df[i, name.x])]
      }
      return(modified.df)
    }

    # encode factors with numbers
    opt.path = encodeFactor(discrete.values, op, name.x)
    plot(c(), xlim=c(0,5), ylim = range(opt.path[,name.y]), type="p", 
      ylab=name.y, main="", xaxt="n")

    axis(1, at=1:length(discrete.values), labels=discrete.values)

    # plot design points    
    plotDesignPoints(opt.path, ind.inides, ind.seqdes, ind.prodes, name.y)
    
    # 3rd plot
    # par(mai=c(0.8, 0.8, 0.1, 0.1))
    # if (model.ok) {
    #   if (proppoints == 1L) {
    #     plot(xseq, evals[, name.crit], xlim=xlim, type="l", lty="dashed", 
    #       xlab=name.x, ylab=name.crit, lwd=lwd.lines, cex.axis=cex.axis, cex.lab=cex.lab)
    #   } else {
    #     if (ctrl$multipoint.method == "lcb") {
    #       ylim = range(evals[, sprintf("lcb_%i", 1:proppoints)])
    #       plot(c(), c(), xlim=xlim, ylim=ylim, 
    #         xlab=name.x, ylab=name.crit, cex.axis=cex.axis, cex.lab=cex.lab)
    #       for (j in 1:proppoints)
    #         lines(xseq, evals[, sprintf("%s_%i", "lcb", j)], lty="dashed", lwd=lwd.lines)
    #     } else if (ctrl$multipoint.method == "multicrit") {
    #       #FIXME add case for bicriteria
    #       plot(xseq, evals[, name.crit], xlim=xlim, type="l", lty="dashed", 
    #         xlab=name.x, ylab=name.crit, lwd=lwd.lines, cex.axis=cex.axis, cex.lab=cex.lab)
    #     }
    #   }
    #   abline(v=op[ind.prodes, name.x])
    #   #plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.crit)
    # } else {
    #   plot.new(); legend("center", "Model fit failed", cex=3)
    # }
    if (pause)
      pause()
  }
}

