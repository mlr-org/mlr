makeOptWrapper = function(id, learner, resampling, measures, par.set, bit.names, bits.to.features,
  control, show.info, learner.subclass, model.subclass) {

  x = makeBaseWrapper(id, learner$type, learner, learner.subclass = c(learner.subclass, "OptWrapper"),
    model.subclass = model.subclass)
  x$resampling = resampling
  x$measures = measures
  x$opt.pars = par.set
  x$bit.names = bit.names
  x$bits.to.features = bits.to.features
  x$opt.pars = par.set
  x$control = control
  x$show.info = show.info
  return(x)
}

#' @export
print.OptModel = function(x, ...) {
  print.WrappedModel(x)
  cat("\nOptimization result:\n")
  print(x$learner.model$opt.result)
}
