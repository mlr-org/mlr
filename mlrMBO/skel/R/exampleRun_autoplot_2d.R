#FIXME we lose space here. x1 and x2 labs can be be put into one legend below plots
# FIXME we probably need a simple gui later to do some trafos like log on the the plots
# while the iters run
autoplotExampleRun2d = function(x, iters, pause=TRUE, densregion=TRUE, 
    se.factor1=1, se.factor2=2, 
    trafo=NULL, ...)  {
      
    # extract information from example run object
    par.set = x$par.set
    names.x = x$names.x
    name.x1 = names.x[1]
    name.x2 = names.x[2]
    name.y = x$name.y
    evals = x$evals
    global.opt = x$global.opt
    control = x$control
    proppoints = control$propose.points
    mbo.res = x$mbo.res
    x1 = unique(evals[, name.x1])
    x2 = unique(evals[, name.x2])
    name.crit = control$infill.crit
    critfun = getInfillCritFunction(name.crit)
    se = (x$learner$predict.type == "se")

    opt.direction = 1
    if (name.crit %in% c("ei")) {
        opt.direction = -1
    }
    opt.path = as.data.frame(mbo.res$opt.path)

    idx.init = which(opt.path$dob == 0)  
      
    # save sequence of opt plots here
    plot.sequence = list()

    # FIXME: what to plot if not infillcrit that uses se?
    # FIXME: how do we display noise? do we at all?
  
    for (i in iters) {
        catf("Iter %i", i)
        model = mbo.res$models[[i]]
    
        idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
        idx.proposed = which(opt.path$dob == i)
        idx.untilnow = c(idx.init, idx.seq, idx.proposed)

        model.ok = !inherits(model, "FailureModel")

        if (model.ok) {
            evals[["yhat"]] = mlrMBO:::infillCritMeanResponse(evals[, names.x, drop=FALSE], 
            model, control, par.set, opt.path[idx.untilnow, ])
            if (se) {
                evals[["se"]] = -mlrMBO:::infillCritStandardError(evals[, names.x, drop=FALSE], 
                model, control, par.set, opt.path[idx.untilnow, ])
            }
            if (proppoints == 1L) {
                evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop=FALSE], 
                model, control, par.set, opt.path[idx.untilnow, ])
            }
        }

        idx = c(idx.init, idx.seq, idx.proposed)

        # helper function for single plot
        plotSingleFun = function(data, points, name.z, xlim, ylim, trafo = NULL) {
            if (!is.null(trafo)) {
                data[, name.z] = trafo(data[, name.z])
            }
            pl = ggplot(data=data, aes_string(x="x1", y="x2", z=name.z))
            pl = pl + geom_tile(aes_string(fill=name.z))
            pl = pl + scale_fill_gradientn(colours = topo.colors(7))
            pl = pl + stat_contour(aes_string(fill=name.z), binwidth=5)
            pl = pl + geom_point(data=points, aes(x=x1, y=x2, z=y, colour=type, shape=type))

            title = name.z
            if (!is.null(trafo)) {
                title = paste(title, " (", attr(trafo, "name"), "-transformed)", sep="")
            }
            
            pl = pl + ggtitle(title)
            pl = pl + scale_colour_manual(name="type", values=c("#000000", "red","gray"))
            pl = pl + xlab(NULL) # remove axis labels
            pl = pl + ylab(NULL)
            pl = pl + theme(
                plot.title=element_text(size=11, face="bold"), # decrease font size and weight
                plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")#, # adapt margins
                #panel.background=element_blank()
            )
            return(pl)
        }

        # make up data structures for ggplot2
        gg.fun = evals
        gg.points = data.frame(
            x1=opt.path[idx, name.x1],
            x2=opt.path[idx, name.x2],
            y=opt.path[idx, name.y],
            type=as.factor(c(
                rep("init", length(idx.init)), 
                rep("seq", length(idx.seq)), 
                rep("prop", length(idx.proposed)))))


        # build single plots
        pl.fun = plotSingleFun(gg.fun, gg.points, "y", trafo=trafo[["y"]])
        pl.mod = plotSingleFun(gg.fun, gg.points, "yhat", trafo=trafo[["yhat"]])
        # FIXME: check why it is "Not possible to generate contour data" for ackley 2d function
        # for the crit and se plots
        pl.crit = plotSingleFun(gg.fun, gg.points, name.crit, trafo=trafo[["crit"]])
        if (se) {
            pl.se = plotSingleFun(gg.fun, gg.points, "se", trafo=trafo[["se"]])
        }

        # make "long" dataframe for ggplot 
        # gg.fun = melt(evals, id.vars=c("x1","x2"))

        # # plot all plots using fancy facets
        # pl.all = ggplot(data=gg.fun, aes(x=x1, y=x2, z=value))
        # pl.all = pl.all + geom_tile(aes(fill=value))
        # pl.all = pl.all + scale_fill_continuous(low="#124874", high="#741212")
        # pl.all = pl.all + geom_point(data=gg.points, aes(x=x1, y=x2, z=y, colour=type))
        # pl.all = pl.all + facet_grid(.~variable, scales="free")
        # pl.all = pl.all + ggtitle(sprintf("Iteration: %i", i))
        # pl.all = pl.all + theme(
        #     plot.title=element_text(size=11, face="bold"),
        #     legend.box = "horizontal", legend.position = "top")
        #pl.all = direct.label(pl.all)
        
        title = sprintf("Iter %i\n x-axis: %s, y-axis: %s", i, name.x1, name.x2)

        if (se) {
            pl.all = grid.arrange(pl.fun, pl.mod, pl.crit, pl.se, 
                nrow=2, 
                main=title)
        } else {
            pl.all = grid.arrange(pl.fun, pl.mod, pl.crit, nrow=1)
        }

        plot.sequence[[i]] = list(
            "pl.fun" = pl.fun,
            "pl.mod" = pl.mod,
            "pl.crit" = pl.crit,
            "pl.se" = if(exists("pl.se")) pl.se else NA,
            "pl.all" = pl.all)

        print(pl.all)

        if (pause) {
            pause()
        }
    }

    return(plot.sequence)
}