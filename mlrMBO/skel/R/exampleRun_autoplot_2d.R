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
    proppoints = control$propose.points
    mbo.res = x$mbo.res
    #col = terrain.colors(255)
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
            if (se) {
                evals[["se"]] = -mlrMBO:::infillCritStandardError(evals[, names.x, drop=FALSE], 
                model, control, par.set, opt.path[idx.untilnow, ])
            }
            if (proppoints == 1L) {
                evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop=FALSE], 
                model, control, par.set, opt.path[idx.untilnow, ])
            }
        }

        # FIXME: move imports elsewhere
        library(reshape2)
        library(gridExtra)

        idx = c(idx.init, idx.seq, idx.proposed)

        # helper function for sinlge fun plots (without facets)
        plotSingleFun = function(data, points, name.z, xlim, ylim) {
            pl = ggplot(data=data, aes_string(x="x1", y="x2", z=name.z))
            pl = pl + geom_tile(aes_string(fill=name.z)) + stat_contour()
            pl = pl + stat_contour(binwidth=5)
            pl = pl + geom_point(data=points, aes(x=x1, y=x2, z=y, colour=type, shape=type))
            pl = pl + ggtitle(sprintf("%s", name.z))
            pl = pl + scale_colour_discrete(name="type")
            pl = pl + scale_fill_continuous(name=name.z)
            pl = pl + theme(plot.title=element_text(size=11, face="bold"))
            #if (name.z %in% c("y")) {
            #    pl = pl + theme(legend.box = "horizontal", legend.position = "top")
            #}
            return(pl)
        }

        # helper to extract legend
        # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
        g_legend <- function(a.gplot){
            tmp <- ggplot_gtable(ggplot_build(a.gplot))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            return(legend)
        }

        # make up data structures for ggplot2
        gg.fun = evals
        gg.points = data.frame(x1=opt.path[idx, name.x1],
                                x2=opt.path[idx, name.x2],
                                y=opt.path[idx, name.y],
                                type=as.factor(c(rep("init", length(idx.init)), 
                                                rep("seq", length(idx.seq)), 
                                                rep("prop", length(idx.proposed)))))

        # build single plots
        pl.fun = plotSingleFun(gg.fun, gg.points, "y")
        pl.mod = plotSingleFun(gg.fun, gg.points, "yhat")
        pl.crit = plotSingleFun(gg.fun, gg.points, name.crit)
        if (se) {
            pl.se = plotSingleFun(gg.fun, gg.points, "se")
        }

        # extract legend from first plot
        gg.legend = g_legend(pl.fun)

        # make "long" dataframe for ggplot 
        gg.fun = melt(evals, id.vars=c("x1","x2"))

        # plot all plots using fancy facets
        pl.all = ggplot(data=gg.fun, aes(x=x1, y=x2, z=value))
        pl.all = pl.all + geom_tile(aes(fill=value))
        pl.all = pl.all + scale_fill_continuous(low="#124874", high="#741212")
        #pl.all = pl.all + stat_contour(aes(fill=..level..))
        #pl.all = pl.all + scale_colour_continuous(low = "#919191", high = "#4d4d4d")
        # FIXME: coloured points not possible, since changing the color needs to change 
        # scale from continuous to discrete. Now we plot each subset of points seperately
        # as a workarount. Unfortunately until now I have no idea how to set the colors
        # in the legend.
        pl.all = pl.all + geom_point(data=gg.points, aes(x=x1, y=x2, z=y, colour=type))
        #pl.all = pl.all + geom_point(data=gg.points[which(as.character(gg.points$type)=="init"),], aes(x=x1, y=x2, z=y, shape=type), size=2.5, colour="#1296ff")
        #pl.all = pl.all + geom_point(data=gg.points[which(as.character(gg.points$type)=="prop"),], aes(x=x1, y=x2, z=y, shape=type), size=3, colour="#f35e5a")
        #pl.all = pl.all + geom_point(data=gg.points[which(as.character(gg.points$type)=="seq"),], aes(x=x1, y=x2, z=y, shape=type), size=3, colour="#18b554")
        pl.all = pl.all + facet_grid(.~variable, scales="free")
        pl.all = pl.all + ggtitle(sprintf("Iteration: %i", i))
        pl.all = pl.all + theme(plot.title=element_text(size=11, face="bold"),
            legend.box = "horizontal", legend.position = "top")
        #pl.all = direct.label(pl.all)
        
        #print(pl.all)
        plot.sequence[[i]] = pl.all

        #print(pl.crit)
        #stop()

        # FIXME: plotting with grid arrange seems ugly if shared legend is neccessary
        if (se) {
            pl = grid.arrange(
                pl.fun,# + theme(legend.position="none"),
                pl.mod,# + theme(legend.position="none"),
                pl.crit,# + theme(legend.position="none"),
                pl.se,# + theme(legend.position="none"),
                nrow=2)
        } else {
            stopf("Not implemented yet.")
        }
        print(pl)

        if (pause) {
            pause()
        }
    }

    return(list(
        "pl.fun" = pl.fun,
        "pl.mod" = pl.mod,
        "pl.crit" = pl.crit,
        "pl.se" = if(exists("pl.se")) pl.se else NA,
        "pl.crit" = pl.crit,
        "pl.all" = pl.all,
        "trace" = plot.sequence))
}