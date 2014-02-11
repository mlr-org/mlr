makeOptResult = function(learner, control, x, y, opt.path, cl) {
  setClasses(list(
    learner = learner,
    control = control,
    x = x,
    y = y,
    opt.path = opt.path
  ), c(cl, "OptResult"))
}
