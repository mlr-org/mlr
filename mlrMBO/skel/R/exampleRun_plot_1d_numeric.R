plotMBOExampleRun1DNumeric = function(x, iters, pause=TRUE, 
  design.pch=c(19,19,19), design.cols=c("black", "darkseagreen", "tomato"), densregion=TRUE, 
  se.factor1=1, se.factor2=2, xlim, ylim, ...)  {
  
  cex.points = 2
  cex.axis = 1.5
  cex.lab = 1.5
  cex.legend = 1.5
  lwd.lines = 2.5
  
  plotDesignPoints = function(op, ind.inides, ind.seqdes, ind.prodes, y) {
    points(op[ind.inides, name.x], op[ind.inides, y], pch=design.pch[[1]], col=design.cols[[1]], cex=cex.points)
    points(op[ind.seqdes, name.x], op[ind.seqdes, y], pch=design.pch[[2]], col=design.cols[[2]], cex=cex.points)
    points(op[ind.prodes, name.x], op[ind.prodes, y], pch=design.pch[[3]], col=design.cols[[3]], cex=cex.points)
  }
  
  requirePackages(c("denstrip"))
  # do we plot stuff relatated to model uncertainty?
  se = x$learner$predict.type == "se"
  par(mfrow=c(2, 1))

  # extract information from example run object
  par.set = x$par.set
  name.x = x$names.x
  name.y = x$name.y
  ctrl = x$control
  minimize = ctrl$minimize 
  evals = x$evals
  evals.x = evals[, name.x, drop=FALSE]
  global.opt = x$global.opt
  xseq = evals[, name.x]
  if (missing(xlim))
    xlim = range(xseq)
  proppoints = ctrl$propose.points
  mr = x$mbo.res
  name.crit = ctrl$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
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
      if (se) {
        evals$se = -mlrMBO:::infillCritStandardError(evals.x,
        mod, ctrl, par.set, op[ind.pasdes, ])
      }
      # infill crit y vals for lower plot
      if (proppoints == 1L) {
        evals[[name.crit]] = opt.direction * critfun(evals.x,
          mod, ctrl, par.set, op[ind.pasdes, ])
      } else {
        if (ctrl$multipoint.method == "lcb") {
          for (j in 1:proppoints) {
            #FIXME works only for lcb
            ctrl2 = ctrl
            ctrl2$infill.crit.lcb.lambda = mr$multipoint.lcb.lambdas[i, j]
            evals[[sprintf("%s_%i", "lcb", j)]] = 
              opt.direction * infillCritLCB(evals.x, 
                mod, ctrl2, par.set, op[ind.pasdes, ])
          }
        } else if (ctrl$multipoint.method == "multicrit") {
          #FIXME add case for bicriteria
          evals[[name.crit]] = -1 * infillCritEI(evals.x,
            mod, ctrl, par.set, op[ind.pasdes, ])
        }
      }
      if (se) {
        evals$yhat.low = evals$yhat - se.factor1 * evals$se 
        evals$yhat.upp = evals$yhat + se.factor1 * evals$se
      }
    }    
    # define layout, i.e., the space available and the order of the plots 
    layout(matrix(c(1, 2, 3), ncol=1, byrow=TRUE), heights=c(0.5, 2.25, 2.25))

    # 1st plots, 2 descriptive legends
    par(mai=c(0, 0.8, 0, 0.1))
    main = sprintf("Iter = %i, Gap = %.4e", i, 
      calculateGap(op[ind.all,], global.opt, ctrl))
    plot.new()
    legend(x="left", legend=main, cex=cex.legend)
    legend(x="right", ncol=2, border="white", 
      legend=c("y", expression(hat(y))),  lty=c("solid", "dotted"), 
      cex=cex.legend, lwd=lwd.lines)

    # 2nd plot, show x-axis on next plot
    
    #FIXME this might "shift" plots while iteration run, nmaybe bad for anims in talks
    # let user define ylim himself?
    #if we have se, set ylim via combo of true y and yhat +- se.factor2*se
    #if we dont have se, simply use xombo of real y and yhat
    if (missing(ylim)) {
      ylim1 = if (se) 
        range(c(evals[, name.y], evals$yhat - se.factor2*evals$se, evals$yhat + se.factor2*evals$se))
      else
        range(c(evals[, name.y], evals$yhat))
    } else {
      ylim1 = ylim
    }

    par(mai=c(0.1, 0.8, 0, 0.1))
    plot(c(), xlim=xlim, ylim=ylim1, 
      xaxt="n", xlab="", ylab=name.y, main="", cex.axis=cex.axis, cex.lab=cex.lab)
    
    if (model.ok && se) {
      if (densregion) {
        dr1 = seq(length = 200, 
          min(evals$yhat - se.factor2 * evals$se), 
          max(evals$yhat + se.factor2 * evals$se)
        )
        # also always eval at yhat, so we should always have nonzero density
        # values per v-line at every point x. otherwise
        # we might sample the density (given x) only at points where it is
        # numerically 0
        dr1 = sort(union(dr1, evals$yhat))
        dr2 = matrix(nrow = length(xseq), ncol = length(dr1))
        for(i in seq_along(xseq)) 
          dr2[i,] = dnorm(dr1, evals$yhat[i], evals$se[i])
        # plot model uncertainty colour gradient
        densregion(xseq, dr1, dr2, pointwise=TRUE, colmax="pink")
      }
      # plot yhat +- se.factor1 * se
      lines(xseq, evals$yhat.low, lty="dotted", lwd=lwd.lines, col=rgb(0, 0, 0, alpha=0.5))
      lines(xseq, evals$yhat.upp, lty="dotted", lwd=lwd.lines, col=rgb(0, 0, 0, alpha=0.5))
    }    
    # plot real objfun
    lines(xseq, evals[, name.y], lwd=lwd.lines)
    if (model.ok) {
      # plot yhat
      lines(xseq, evals$yhat, lty="dotted", lwd=lwd.lines)
      #FIXME what about noise on real evals during mbo? show this how? plot real evals??
    }
    # plot design points    
    plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.y)
    
    # 3rd plot
    par(mai=c(0.8, 0.8, 0.1, 0.1))
    if (model.ok) {
      if (proppoints == 1L) {
        plot(xseq, evals[, name.crit], xlim=xlim, type="l", lty="dashed", 
          xlab=name.x, ylab=name.crit, lwd=lwd.lines, cex.axis=cex.axis, cex.lab=cex.lab)
      } else {
        if (ctrl$multipoint.method == "lcb") {
          ylim = range(evals[, sprintf("lcb_%i", 1:proppoints)])
          plot(c(), c(), xlim=xlim, ylim=ylim, 
            xlab=name.x, ylab=name.crit, cex.axis=cex.axis, cex.lab=cex.lab)
          for (j in 1:proppoints)
            lines(xseq, evals[, sprintf("%s_%i", "lcb", j)], lty="dashed", lwd=lwd.lines)
        } else if (ctrl$multipoint.method == "multicrit") {
          #FIXME add case for bicriteria
          plot(xseq, evals[, name.crit], xlim=xlim, type="l", lty="dashed", 
            xlab=name.x, ylab=name.crit, lwd=lwd.lines, cex.axis=cex.axis, cex.lab=cex.lab)
        }
      }
      abline(v=op[ind.prodes, name.x])
      #plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.crit)
    } else {
      plot.new(); legend("center", "Model fit failed", cex=3)
    }
    if (pause)
      pause()
  }
}

