makeOptResult = function(learner, control, x, y, threshold, opt.path, cl) {
  setClasses(list(
    learner = learner,
    control = control,
    x = x,
    y = y,
    threshold = threshold,
    opt.path = opt.path
  ), c(cl, "OptResult"))
}
