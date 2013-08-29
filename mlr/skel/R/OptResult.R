makeOptResult = function(learner, control, x, y, opt.path, cl) {
  structure(list(
    learner = learner,       
    control = control,			
		x = x,
    y = y,
    opt.path = opt.path
  ), class=c(cl, "OptResult"))
}

