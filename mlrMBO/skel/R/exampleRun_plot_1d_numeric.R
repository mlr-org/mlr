#FIXME x and y labels are not shown
plotMBOExampleRun1DNumeric = function(obj, ...,  iters, pch, col.initdes, col.seqdes, col.propdes) {
  
  points.cex = 2
  lines.lwd = 2.5
  axis.cex = 1.5
  plotDesignPoints = function(op, ind.inides, ind.seqdes, ind.prodes, y) {
    points(op[ind.inides, name.x], op[ind.inides, y], pch=pch, col=col.initdes, cex=points.cex)
    points(op[ind.seqdes, name.x], op[ind.seqdes, y], pch=pch, col=col.seqdes, cex=points.cex)
    points(op[ind.prodes, name.x], op[ind.prodes, y], pch=pch, col=col.propdes, cex=points.cex)
  }
  
  requirePackages(c("denstrip"))
  # do we plot stuff relatated to model uncertainty?
  se = obj$learner$predict.type == "se"
  par(mfrow=c(2, 1))

  # extract information from example run object
  par.set = obj$par.set
  name.x = obj$names.x
  name.y = obj$name.y
  evals = obj$evals
  evals.x = evals[, name.x, drop=FALSE]
  xseq = evals[, name.x]
  name.crit = obj$control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
  op = as.data.frame(obj$mbo.res$opt.path)
  # ind.* are index sets into the opt.path (initial design and so on)
  ind.inides = which(op$dob == 0)
  
  for (i in iters) {
    mod = obj$mbo.res$models[[i]]
    
    ind.seqdes = which(op$dob > 0 & op$dob < i)
    ind.prodes = which(op$dob == i)
    ind.pasdes = c(ind.inides, ind.seqdes)
    
    # compute model prediction for current iter
    evals$yhat = mlrMBO:::infillCritMeanResponse(evals.x, 
      mod, ctrl, par.set, op[ind.pasdes, ])
    if (se) {
    evals$se = -mlrMBO:::infillCritStandardError(evals.x,
      mod, ctrl, par.set, op[ind.pasdes, ])
    }
    evals[[name.crit]] = opt.direction * critfun(evals.x,
      mod, ctrl, par.set, op[ind.pasdes, ])
    if (se) {
      evals$yhat.low = evals$yhat - 1 * evals$se 
      evals$yhat.upp = evals$yhat + 1 * evals$se
    }

    # infill crit y vals for lower plot
    op[[name.crit]] = opt.direction * critfun(op[, name.x, drop =FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    
    # define layout, i.e., the space available and the order of the plots 
    layout(matrix(c(1, 2, 3), ncol=1, byrow=TRUE), heights=c(2.25, 2.25, 0.5))

    par(mai=c(0.15, 0.8, 0.3, 0.2))
    #FIXME check range
    plot(c(), xlim = range(xseq), ylim = range(evals[, name.y]), 
      xlab=name.x, ylab="", main=sprintf("Iter = %i", i), cex.axis=axis.cex)

    if (se) {
      dr1 = seq(length = 200, 
        min(evals$yhat - 2.5 * evals$se), 
        max(evals$yhat + 2.5 * evals$se)
      )
      dr2 = matrix(nrow = length(xseq), ncol = length(dr1))
      for(i in seq_along(xseq)) 
        dr2[i,] = dnorm(dr1, evals$yhat[i], evals$se[i])
      rowmaxs = apply(dr2, 1, max)
      # FIXME really dirty fix if uncertainty becomes really low
      for (j in seq_row(dr2)) {
        if (rowmaxs[j] < 1e-12) {
          dr2[j, ] = dnorm(dr1, evals$yhat[j], 0.001)
        }
      }
      # plot model uncertainty colour gradient
      densregion(xseq, dr1, dr2, pointwise=TRUE, colmax="pink")
      # plot yhat +- 1 se
      lines(xseq, evals$yhat.low, lty="dotted", lwd=lines.lwd, col=rgb(0, 0, 0, alpha=0.5))
      lines(xseq, evals$yhat.upp, lty="dotted", lwd=lines.lwd, col=rgb(0, 0, 0, alpha=0.5))
    }    
    # plot real objfun
    lines(xseq, evals[, name.y], lwd = lines.lwd)
    # plot yhat
    lines(xseq, evals$yhat, lty="dotted", lwd=lines.lwd)
    #FIXME what about noise on real evals during mbo? show this how? plot real evals??
    # plot design points    
    plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.y)
    
    #par(mai=c(0.15, 0.8, 0.15, 0.2))
    plot(xseq, evals[, name.crit], type="l", lty="dashed", 
      xlab=name.x, ylab="", lwd=lines.lwd, cex.axis=axis.cex)
    plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.crit)
    # add legend in seperate layout row
    par(mai=c(0, 0, 0.2, 0))
    plot.new()
    legend(x="center", ncol=3, border="white", legend=c("y", expression(hat(y)), name.crit), 
      lty=c("solid", "dotted", "dashed"), cex=1.5, lwd=lines.lwd)
    pause()
  }
}

    # ggplot playground 
    # library(ggplot2)
    # library(gridExtra)
    # finegrid2 = data.frame(x=rep(finegrid$x, 2))
    # finegrid2$y = c(finegrid$y, finegrid$yhat)
    # finegrid2$f = rep(c("y", "yhat"), each=nrow(finegrid))
    # finegrid2$yhat.low = rep(finegrid$yhat.low, 2)
    # finegrid2$yhat.upp = rep(finegrid$yhat.upp, 2)
    # #print(head(finegrid2))
    # # FIXME: insert infill crit in legend, place legend
    # pl1 = ggplot(data=finegrid2, aes(x=x, y=y, linetype=f)) + geom_line()
    # pl1 = pl1 + ggtitle(paste("Iteration", i))
    # pl1 = pl1 + theme(legend.position="none")
    # pl1 = pl1 + geom_ribbon(aes(ymin=yhat.low, ymax=yhat.upp), alpha=0.1, colour ="gray", linetype="dashed", fill="red")
    # pl2 = ggplot(data=finegrid, aes(x=x, y=crit)) + geom_line(linetype="dashed")
    # grid.arrange(pl1, pl2, nrow=2)
    # stop()

