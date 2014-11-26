makeBaseWrapper = function(id, next.learner, package = character(0L), par.set = makeParamSet(),
  par.vals = list(), learner.subclass, model.subclass) {

  if (inherits(next.learner, "OptWrapper"))
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(next.learner$par.set$pars))
  if (length(ns) > 0L)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))

  makeS3Obj(c(learner.subclass, "BaseWrapper", "Learner"),
    id = id,
    type = next.learner$type,
    predict.type = next.learner$predict.type,
    package = union(package, next.learner$package),
    par.set = par.set,
    par.vals = par.vals,
    properties = next.learner$properties,
    fix.factors = FALSE,
    next.learner = next.learner,
    model.subclass = model.subclass
  )
}

#' @export
print.BaseWrapper = function(x, ...) {
  s = ""
  y = x
  while (inherits(y, "BaseWrapper")) {
    s = paste(s, class(y)[1L], "->", sep = "")
    y = y$next.learner
  }
  s = paste(s, class(y)[1L])
  print.Learner(x)
}

#' @export
predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  args = removeFromDots(names(.learner$par.vals), ...)
  do.call(predictLearner, c(
    list(.learner = .learner$next.learner, .model = .model$learner.model$next.model, .newdata = .newdata),
    args)
  )
}

#' @export
makeWrappedModel.BaseWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, c(learner$model.subclass, "BaseWrapperModel"))
}

##############################           BaseWrapperModel                 ##############################

#' @export
isFailureModel.BaseWrapperModel = function(model) {
  return(isFailureModel(model$learner.model$next.model))
}
