makeOptResult = function(learner, control, x, y, threshold, opt.path, cl, ...) {
  res = list(
    learner = learner,
    control = control,
    x = x,
    y = y,
    threshold = threshold,
    opt.path = opt.path
  )
  res = c(res, list(...))
  setClasses(res, c(cl, "OptResult"))
}
