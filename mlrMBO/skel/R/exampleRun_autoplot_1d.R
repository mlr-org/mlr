autoplotExampleRun1d = function(x, iters, xlim, ylim, pause, ...) {
	# FIXME: add comments
	# FIXME: add supoort for 1d with discrete param (keep code clean and try to overgo redundant code)
	# FIXME: gridExtra not ready for R 3.x. What is the alternative?
	par.set = x$par.set
	par.types = x$par.types 
	names.x = x$names.x
	name.y = x$name.y
	control = x$control
	noisy = control$noisy
	mbo.res = x$mbo.res
	models = mbo.res$models

	se = (x$learner$predict.type == "se")

	propose.points = ctrl$propose.points
	name.crit = ctrl$infill.crit
  	critfun = getInfillCritFunction(name.crit)
  	opt.direction = 1
  	if (name.crit %in% c("ei")) {
    	opt.direction = -1
  	}

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

	if (par.types %in% c("numeric", "numericvector")) {
		#stopf("Plotting 1d numeric function.")
		for (i in 1:iters) {

			# FIXME: the following lines work for discrete parameter as well.
			# FIXME: only the constuction of the "gg" dataframe is special
			# FIXME: add iteration counter and further meta information
			# FIXME: keep in mind geom_errorbar for 
			model = models[[i]]
			idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
			idx.proposed = which(opt.path$dob == i)
			idx.untilnow = c(idx.seq, idx.proposed)

			model.ok = !inherits(model, "FailureModel")
	    
	    	# compute model prediction for current iter
	    	if (model.ok) {
	      		evals$yhat = mlrMBO:::infillCritMeanResponse(evals.x, model, 
	      			control, par.set, opt.path[idx.untilnow,])
	        }

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

	        n = nrow(evals)
	        # FIXME: checkout the functionality of the with(...) construct
	        gg.fun = data.frame(x=rep(evals[,names.x],2), 
	        	                y=c(evals[,name.y],evals[,"yhat"]),
	        	                se.min=if (se) rep(evals[,"se.min"], 2) else NA,
	        	                se.max=if (se) rep(evals[,"se.max"], 2) else NA,
	        	                type=as.factor(rep(c("y", "yhat"), each=n)))

	        gg.crit = data.frame(x=evals[,names.x],
	        	                 y=evals[,name.crit])
	        #print(head(gg.fun))
	        pl.fun = ggplot(data=gg.fun)
	        pl.fun = pl.fun + geom_line(aes(x=x, y=y,linetype=type))
	        if (se) {
	        	# FIXME: rename "gg.funss"
	        	gg.funss = subset(gg.fun, type=="yhat")
	        	#head(gg.funss)
	        	pl.fun = pl.fun + geom_ribbon(data=gg.funss, aes(x=x, ymin=se.min, ymax=se.max), alpha=0.2)
	        }
	        #pl.fun = pl.fun + geom_point(data=opt.path[idx.init,], aes_string(x=names.x, y=name.y), colour="black", alpha=.4)

	        # build dataframe for different points
	        idx = c(idx.init, idx.seq, idx.proposed)
	        gg.points = data.frame(x=opt.path[idx, names.x],
	        	                   y=opt.path[idx, name.y],
	        	                   type=as.factor(c(rep("init", length(idx.init)), rep("seq", length(idx.seq)), rep("prop", length(idx.proposed)))))
	        print(head(gg.points))
	        # if (length(idx.seq) > 0) {
	        # 	pl.fun = pl.fun + geom_point(data=opt.path[idx.seq,], aes_string(x=names.x, y=name.y), colour="green")
	        # }
	        # pl.fun = pl.fun + geom_point(data=opt.path[idx.proposed,], aes_string(x=names.x, y=name.y), colour="tomato", size=3)
	        # #pl.fun = pl.fun + geom_hline(yintercept=global.opt, linetype="dashed", colour="darkgray")
	        pl.fun = pl.fun + geom_point(data=gg.points, aes(x=x, y=y, colour=type))
	        pl.fun = pl.fun + theme(legend.position="top", legend.box = "horizontal", axis.text.x=element_blank())
	        
	        pl.crit = ggplot(data=gg.crit, aes(x=x, y=y))
	        pl.crit = pl.crit + geom_line(linetype="dotted", colour="darkgray")
	        pl.crit = pl.crit + geom_vline(xintercept=opt.path[idx.untilnow, names.x])
	        pl.crit = pl.crit + scale_y_continuous(name=name.crit)

	        grid.arrange(pl.fun, pl.crit, nrow=2, main=paste("Visualization of 1d numeric function optimization.\n Iter: ", i))

	        if (pause) {
				pause()
			}
		}
	} else if (par.types %in% c("discrete")) {
		if (!noisy) {
			stopf("Deterministic 1d function with a single factor parameter are not supported.")
		}
		stopf("Plotting 1d noisy discrete function.")
	}
}