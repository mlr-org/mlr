#FIXME we lose space here. x1 and x2 labs can be be put into one legend below plots
# FIXME we probably need a simple gui later to do some trafos like log on the the plots
# while the iters run
autoplotExampleRun2d = function(x, iters, pause=TRUE, densregion=TRUE, 
    se.factor1=1, se.factor2=2, ...)  {
      
    # extract information from example run object
    par.set = x$par.set
    names.x = x$names.x
    name.x1 = names.x[1]
    name.x2 = names.x[2]
    name.y = x$name.y
    evals = x$evals
    global.opt = x$global.opt
    control = x$control
    proppoints = ctrl$propose.points
    mbo.res = x$mbo.res
    #col = terrain.colors(255)
    x1 = unique(evals[, name.x1])
    x2 = unique(evals[, name.x2])
    name.crit = ctrl$infill.crit
    critfun = getInfillCritFunction(name.crit)
    opt.direction = 1
    if (name.crit %in% c("ei")) {
        opt.direction = -1
    }
    opt.path = as.data.frame(mbo.res$opt.path)

    idx.init = which(opt.path$dob == 0)  
      
    # FIXME: add option to log
    # FIXME: add se option whether it is plotted
    # FIXME: what to plot if not infillcrit that uses se?
    # FIXME: how do we display noise? do we at all?
  
    for (i in 2:iters) {
        catf("Iter %i", i)
        model = mbo.res$models[[i]]
    
        idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
        idx.proposed = which(opt.path$dob == i)
        idx.untilnow = c(idx.init, idx.seq, idx.proposed)

        model.ok = !inherits(model, "FailureModel")
    
        if (model.ok) {
            evals[["yhat"]] = mlrMBO:::infillCritMeanResponse(evals[, names.x, drop=FALSE], 
            model, control, par.set, opt.path[idx.untilnow, ])
            # if (se) 
            #     evals[["se"]] = -mlrMBO:::infillCritStandardError(evals[, names.x, drop=FALSE], 
            #     model, control, par.set, opt.path[ind.pasdes, ])
            if (proppoints == 1L) {
                evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop=FALSE], 
                model, control, par.set, opt.path[idx.untilnow, ])
            }
        }

        # FIXME: move import elsewhere
        library(reshape2)
        idx = c(idx.init, idx.seq, idx.proposed)

        print(head(evals))
        # make "long" ggplot-friendly data frame out of "wide" one
        gg.fun = melt(evals, id=c("x1", "x2"), value.name="y")
        print(head(gg.fun))
        # FIXME: fill=z issues error. Check out what is exactly going wrong!
        pl.fun = ggplot(data=gg.fun, aes(x=x1,y=x2,z=y))
        pl.fun = pl.fun + geom_tile(aes(fill=y))
        #pl.fun = pl.fun + scale_fill_gradient(low="white", high="grey40")
        #pl.fun = pl.fun + stat_contour(binwidth=4, size=0.5, colour="grey30")
        pl.fun = pl.fun + stat_contour(binwidth=12, size=0.5, colour="grey50")
        pl.fun = pl.fun + facet_grid(. ~ variable, scales="free")
        #pl.fun = pl.fun + geom_point(data=gg.fun[idx,])

        gg.points = data.frame(x1=opt.path[idx, name.x1],
                                x2=opt.path[idx, name.x2],
                                y=opt.path[idx, name.y],
                                type=as.factor(c(rep("init", length(idx.init)), 
                                                rep("seq", length(idx.seq)), 
                                                rep("prop", length(idx.proposed)))))

        pl.fun = pl.fun + geom_point(data=gg.points, aes(x=x1, y=x2, z=y, colour=type))
        pl.fun = pl.fun + ggtitle(sprintf("Iter: %i", i))
        pl.fun = pl.fun + theme(plot.title=element_text(size=11, face="bold"),
            legend.box = "horizontal",
            legend.position = "top")
                        
        print(pl.fun)
        if (pause) {
            pause()
        }
    }
}