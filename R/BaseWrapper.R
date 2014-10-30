makeBaseWrapper = function(id, next.learner, package = character(0L), par.set = makeParamSet(),
  par.vals = list(), cl) {

  if (inherits(next.learner, "OptWrapper"))
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(next.learner$par.set$pars))
  if (length(ns) > 0L)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))

  makeS3Obj(c(cl, "BaseWrapper", "Learner"),
    id = id,
    type = next.learner$type,
    predict.type = next.learner$predict.type,
    package = union(package, next.learner$package),
    par.set = par.set,
    par.vals = par.vals,
    properties = next.learner$properties,
    fix.factors = FALSE,
    next.learner = next.learner
  )
}

#' @export
predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  args = removeFromDots(names(.learner$par.vals), ...)
  do.call(predictLearner, c(
    list(.learner = .learner$next.learner, .model = .model$learner.model$next.model, .newdata = .newdata),
    args)
  )
}

# FIXME: test
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
