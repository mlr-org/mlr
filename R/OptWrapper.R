makeOptWrapper = function(id, learner, resampling, measures, par.set, bit.names, bits.to.features,
  control, show.info, cl) {

  x = makeBaseWrapper(id, learner, cl=c(cl, "OptWrapper"))
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
makeWrappedModel.OptWrapper = function(learner, model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  if (inherits(learner, "TuneWrapper"))
    addClasses(x, c("TuneModel", "OptModel"))
  else
    addClasses(x, c("FeatSelModel", "OptModel"))
}

#' @export
print.OptModel = function(x, ...) {
  print.WrappedModel(x)
  cat("\nOptimization result:\n")
  print(x$learner.model$opt.result)
}
