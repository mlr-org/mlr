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

#' @S3method makeWrappedModel OptWrapper
makeWrappedModel.OptWrapper = function(learner, model, task.desc, subset, features, time) {
  x = NextMethod()
  class(x) = c("TuneModel", "OptModel", class(x))
  return(x)
}

#' @S3method print OptModel
print.OptModel = function(x, ...) {
  print.WrappedModel(x)
  cat("\nOptimization result:\n")
  print(x$learner.model$opt.result)
}
