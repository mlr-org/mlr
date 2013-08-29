# FIXME: test this
makeBaseWrapper = function(id, next.learner, package=character(0), par.set=makeParamSet(), 
  par.vals=list(), cl) {

  if (inherits(next.learner, "OptWrapper")) 
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(next.learner$par.set$pars))
  if (length(ns) > 0)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))
  
  structure(list(
    id = id,
    type = next.learner$type,
    package = c(package, next.learner$package),
    par.set = par.set,
    par.vals = par.vals,
    numerics = next.learner$numerics,
    factors = next.learner$factors,
    predict.type = next.learner$predict.type,
    missings = next.learner$missings,
    weights = next.learner$weights,
    oneclass = next.learner$oneclass,
    twoclass = next.learner$twoclass,
    multiclass = next.learner$multiclass,
    prob = next.learner$prob,
    se = next.learner$se,
    next.learner = next.learner    
  ), class = c(cl, "BaseWrapper", "Learner"))
}

#' @S3method predictLearner BaseWrapper
predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  args = removeFromDots(names(.learner$par.vals), ...)  
  do.call(predictLearner, c(
    list(.learner$next.learner, .model$learner.model$next.model, .newdata), 
    args))   
}

# FIXME: test
#' @S3method print BaseWrapper
print.BaseWrapper = function(x, ...) {
  s = ""
  y = x
  while (inherits(y, "BaseWrapper")) {
    s = paste(s, class(y)[1], "->", sep="")
    y = y$next.learner
  }
  s = paste(s, class(y)[1])
  print.Learner(x)
}

