# FIXME: add function description
# FIMXE: check if param names agree with regular plot function
autoplotExampleRun1d = function(x, iters, xlim, ylim, pause, point.size, densregion=TRUE...) {
	# FIXME: add comments
	# FIXME: add support for 1d with discrete param (keep code clean and try to overgo redundant code)
	par.set = x$par.set
	par.types = x$par.types 
	names.x = x$names.x
	name.y = x$name.y
	control = x$control
	noisy = control$noisy
	mbo.res = x$mbo.res
	models = mbo.res$models

	se = (x$learner$predict.type == "se")

	propose.points = control$propose.points
	name.crit = control$infill.crit
  	critfun = getInfillCritFunction(name.crit)
  	opt.direction = 1

  	# we need to maximize expected improvement
  	if (name.crit %in% c("ei")) {
    	opt.direction = -1
  	}

  	# if no iterations provided take the total number of iterations in optimization process
	if(missing(iters)) {
		iters = length(models)
	}

	global.opt = x$global.opt
	if (is.na(global.opt)) {
		global.opt = x$global.opt.estim
	}

	evals = x$evals
  	evals.x = evals[, names.x, drop=FALSE]

	opt.path = as.data.frame(mbo.res$opt.path)

	idx.init = which(opt.path$dob == 0)

	
	#stopf("Plotting 1d numeric function.")
	for (i in iters) {
		catf("Iter %i", i)

		# FIXME: the following lines work for discrete parameter as well.
		# FIXME: only the constuction of the "gg" dataframe is special
		model = models[[i]]
		idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
		idx.proposed = which(opt.path$dob == i)
		idx.untilnow = c(idx.seq, idx.proposed)

		model.ok = !inherits(model, "FailureModel")
    
    	# compute model prediction for current iter
    	if (model.ok) {
      		evals$yhat = mlrMBO:::infillCritMeanResponse(evals.x, model, 
      			control, par.set, opt.path[idx.untilnow,])

      		if (propose.points == 1L) {
    		evals[[name.crit]] = opt.direction * critfun(evals.x,
      			model, control, par.set, opt.path[idx.untilnow,])
       		}

       		# prepare drawing of standard error (confidence interval)
        	if (se) {
    			evals$se = -mlrMBO:::infillCritStandardError(evals.x, model,
    				control, par.set, opt.path[idx.untilnow,])
    			# FIXME: make a parameter out of the constant factor
    			evals$se.min = evals$yhat - 0.3 * evals$se
    			evals$se.max = evals$yhat + 0.3 * evals$se
  			}
        }
        
        # FIXME: better source the actual ploting out in seperate numeric/discrete files
  		if (par.types %in% c("numeric", "numericvector")) {
	  		# ggplot stuff
	        n = nrow(evals)

	        # data frame with real fun and model fun evaluations
	        gg.fun = data.frame(x=rep(evals[,names.x],2), 
	        	                y=c(evals[,name.y],evals[,"yhat"]),
	        	                se.min=if (se) rep(evals[,"se.min"], 2) else NA,
	        	                se.max=if (se) rep(evals[,"se.max"], 2) else NA,
	        	                type=as.factor(rep(c("y", "yhat"), each=n)))

	        # data frame with optimization criterion stuff
	        gg.crit = data.frame(x=evals[,names.x],
	        	                 y=evals[,name.crit])

	        # build dataframe for different points
	        idx = c(idx.init, idx.seq, idx.proposed)
	        # FIXME: the constuction of type is ugly. Maybe identify a pattern and make a useful function
	        gg.points = data.frame(x=opt.path[idx, names.x],
	        	                   y=opt.path[idx, name.y],
	        	                   type=as.factor(c(rep("init", length(idx.init)), 
	        	                   					rep("seq", length(idx.seq)), 
	        	                   					rep("prop", length(idx.proposed)))))

	        # actual ploting stuff
	        pl.fun = ggplot(data=gg.fun)
	        pl.fun = pl.fun + geom_line(aes(x=x, y=y,linetype=type))
	        if (se & densregion) {
	        	gg.se = subset(gg.fun, type=="yhat")
	        	pl.fun = pl.fun + geom_ribbon(data=gg.se, aes(x=x, ymin=se.min, ymax=se.max), alpha=0.2)
	        }

	        pl.fun = pl.fun + geom_point(data=gg.points, aes(x=x, y=y, colour=type), size=point.size)
	        pl.fun = pl.fun + xlab(NULL)
	        pl.fun = pl.fun + ggtitle(sprintf("Iter = %i, Gap = %.4e", i, 
	        	calculateGap(opt.path[idx.untilnow,], global.opt, control)))
	        pl.fun = pl.fun + theme(legend.position="top", 
	        	legend.box = "horizontal", 
	        	axis.text.x=element_blank(),
	        	panel.margin=unit(0, "lines"),
	        	plot.title=element_text(size=11, face="bold"))
	       
	        pl.crit = ggplot(data=gg.crit, aes(x=x, y=y))
	        pl.crit = pl.crit + geom_line(linetype="dotted", colour="black")
	        pl.crit = pl.crit + geom_vline(xintercept=opt.path[idx.proposed, names.x], linetype="dashed", colour="darkgray")
	        pl.crit = pl.crit + scale_y_continuous(name=name.crit)

	      	# arrange stuff in grid
	      	# FIXME: maybe better return list of ggplot objects?
	      	# FIXME: plots are not exactly arranged. There is a small shift in horizontal direction.
	        grid.arrange(pl.fun, pl.crit, nrow=2)
		} else if (par.types %in% c("discrete")) {
			if (!noisy) {
				stopf("Deterministic 1d function with a single factor parameter are not supported.")
			}

			# build dataframe for different points
	        idx = c(idx.init, idx.seq, idx.proposed)

	        # FIXME: this is copy and paste (see numeric part)
	        gg.points = data.frame(x=opt.path[idx, names.x],
	        	                   y=opt.path[idx, name.y],
	        	                   type=as.factor(c(rep("init", length(idx.init)), 
	        	                   					rep("seq", length(idx.seq)), 
	        	                   					rep("prop", length(idx.proposed)))))
			head(gg.points)
			
    		pl.points = ggplot(data=gg.points, aes(x=x, y=y, colour=type, shape=type)) + geom_point(size=point.size)
    		pl.points = pl.points + xlab(names.x)
    		pl.points = pl.points + ylab(name.y)
			pl.points = pl.points + scale_colour_discrete(name="type")
    		pl.points = pl.points + ggtitle(sprintf("Iter = %i, Gap = %.4e", i, 
	        	calculateGap(opt.path[idx.untilnow,], global.opt, control)))
    		pl.points = pl.points + theme(legend.position="top", 
	        	legend.box = "horizontal", 
	        	plot.title=element_text(size=11, face="bold"))
    		print(pl.points)
		}

        if (pause) {
			pause()
		}
	}
}