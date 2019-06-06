#' Exported for internal use only.
#' @param id (`character(1)`)\cr
#'   Id string for object. Used to display object.
#' @param type (`character(1)`)\cr
#'   Learner type.
#' @param next.learner ([Learner])\cr
#'   Learner to wrap.
#' @param package ([character])\cr
#'   Packages to load when loading learner.
#' @param par.set ([ParamSet])\cr
#'   Parameter set.
#' @param par.vals ([list])\cr
#'   Optional list of named (hyper)parameter values.
#' @param learner.subclass ([character])\cr
#'   Class to assign the new object.
#' @param model.subclass ([character])\cr
#'   Class to assign learner models.
#' @keywords internal
#' @export
makeBaseWrapper = function(id, type, next.learner, package = character(0L), par.set = makeParamSet(),
  par.vals = list(), learner.subclass, model.subclass, cache = FALSE) {

  if (inherits(next.learner, "OptWrapper") && is.element("TuneWrapper", learner.subclass)) {
    stop("Cannot wrap a tuning wrapper around another optimization wrapper!")
  }
  ns = intersect(names(par.set$pars), names(next.learner$par.set$pars))
  if (length(ns) > 0L) {
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))
  }

  learner = makeLearnerBaseConstructor(classes = c(learner.subclass, "BaseWrapper"),
    id = id,
    type = type,
    predict.type = next.learner$predict.type,
    package = union(package, next.learner$package),
    properties = NULL, # these are handled by the getter anyway
    par.set = par.set,
    par.vals = par.vals,
    cache = cache
  )
  learner$fix.factors.prediction = FALSE
  learner$next.learner = next.learner
  learner$model.subclass = model.subclass
  return(learner)
}

#' @export
print.BaseWrapper = function(x, ...) {
  s = ""
  y = x
  while (inherits(y, "BaseWrapper")) {
    s = stri_paste(s, class(y)[1L], "->", sep = "")
    y = y$next.learner
  }
  s = stri_paste(s, class(y)[1L], sep = " ")
  print.Learner(x)
}


# trainLearner:
# trainLearner.BaggingWrapper = function(.learner, .task, .subset, .weights = NULL, ...)
# trainLearner is not implemented here, as no concrete code makes sense for this abstract base method.
# One word wrt. hyper pars and the "..." varargs in inheriting methods:
# train calls trainLearner, then passes ALL par.vals from all wrapped learner layers to "...".
# In trainLearner.MyWrapper the params that are of interest are usually bound by name.
# Then in trainLearner.MyWrapper usually "train" is called for next.learner.
# This does not accept "..." and we do not pass "..." down any further.
# But we do not need to do this, as the settings of learner$next.learner are
# contained in next.learner$par.vals.


#' @export
predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  args = removeFromDots(names(.learner$par.vals), ...)
  do.call(predictLearner, c(
    list(.learner = .learner$next.learner, .model = .model$learner.model$next.model, .newdata = .newdata),
    args)
  )
}

#' @export
makeWrappedModel.BaseWrapper = function(learner, learner.model, task.desc, subset = NULL, features, factor.levels, time) {
  x = NextMethod()
  if (!isFailureModel(x)) {
    x = addClasses(x, c(learner$model.subclass, "BaseWrapperModel"))
  }
  return(x)
}

##############################           BaseWrapperModel                 ##############################

#' @export
isFailureModel.BaseWrapperModel = function(model) {
  return(!inherits(model$learner.model, "NoFeaturesModel") && isFailureModel(model$learner.model$next.model))
}

#' @export
getFailureModelMsg.BaseWrapperModel = function(model) {
  return(getFailureModelMsg(model$learner.model$next.model))
}

#' @export
getFailureModelDump.BaseWrapperModel = function(model) {
  return(getFailureModelDump(model$learner.model$next.model))
}

#' @export
getLearnerProperties.BaseWrapper = function(learner) {
  # set properties by default to what the resulting type is allowed and what the base learner can do
  intersect(listLearnerProperties(learner$type), getLearnerProperties(learner$next.learner))
}
