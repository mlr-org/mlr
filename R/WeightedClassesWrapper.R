#' @title Wraps a classifier for weighted fitting where each class receives a weight.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#'
#' Fitting is performed in a weighted fashion where each observation receives a weight,
#' depending on the class it belongs to, see `wcw.weight`.
#' This might help to mitigate problems caused by imbalanced class distributions.
#'
#' This weighted fitting can be achieved in two ways:
#'
#' a) The learner already has a parameter for class weighting, so one weight can directly be defined
#' per class. Example: \dQuote{classif.ksvm} and parameter `class.weights`.
#' In this case we don't really do anything fancy. We convert `wcw.weight` a bit,
#' but basically simply bind its value to the class weighting param.
#' The wrapper in this case simply offers a convenient, consistent fashion for class weighting -
#' and tuning! See example below.
#'
#' b) The learner does not have a direct parameter to support class weighting, but
#' supports observation weights, so `hasLearnerProperties(learner, 'weights')` is `TRUE`.
#' This means that an individual, arbitrary weight can be set per observation during training.
#' We set this weight depending on the class internally in the wrapper. Basically we introduce
#' something like a new \dQuote{class.weights} parameter for the learner via observation weights.
#'
#' @template arg_learner_classif
#' @param wcw.param (`character(1)`)\cr
#'   Name of already existing learner parameter, which allows class weighting.
#'   The default (`wcw.param = NULL`) will use the parameter defined in
#'   the learner (`class.weights.param`). During training, the parameter
#'   must accept a named vector of class weights, where length equals the
#'   number of classes.
#' @param wcw.weight ([numeric])\cr
#'   Weight for each class.
#'   Must be a vector of the same number of elements as classes are in task,
#'   and must also be in the same order as the class levels are in
#'   `getTaskDesc(task)$class.levels`.
#'   For convenience, one must pass a single number in case of binary classification, which
#'   is then taken as the weight of the positive class, while the negative class receives a weight
#'   of 1.
#'   Default is 1.
#' @template ret_learner
#' @family wrapper
#' @export
#' @examples
#' \donttest{
#' set.seed(123)
#' # using the direct parameter of the SVM (which is already defined in the learner)
#' lrn = makeWeightedClassesWrapper("classif.ksvm", wcw.weight = 0.01)
#' res = holdout(lrn, sonar.task)
#' print(calculateConfusionMatrix(res$pred))
#'
#' # using the observation weights of logreg
#' lrn = makeWeightedClassesWrapper("classif.logreg", wcw.weight = 0.01)
#' res = holdout(lrn, sonar.task)
#' print(calculateConfusionMatrix(res$pred))
#'
#' # tuning the imbalancy param and the SVM param in one go
#' lrn = makeWeightedClassesWrapper("classif.ksvm", wcw.param = "class.weights")
#' ps = makeParamSet(
#'   makeNumericParam("wcw.weight", lower = 1, upper = 10),
#'   makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
#'   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
#' )
#' ctrl = makeTuneControlRandom(maxit = 3L)
#' rdesc = makeResampleDesc("CV", iters = 2L, stratify = TRUE)
#' res = tuneParams(lrn, sonar.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' # print(res$opt.path)
#' }
makeWeightedClassesWrapper = function(learner, wcw.param = NULL, wcw.weight = 1) {

  learner = checkLearner(learner, "classif")
  pv = list()

  if (is.null(wcw.param)) {
    wcw.param = learner$class.weights.param
  } else if (!is.null(learner$class.weights.param) && (learner$class.weights.param != wcw.param)) {
    stopf("wcw.param (%s) differs from the class.weights.parameter (%s) of the learner!",
      wcw.param, learner$class.weights.param)
  }

  if (is.null(wcw.param)) {
    if (!hasLearnerProperties(learner, "weights")) {
      stopf("Learner '%s' does not support observation weights. You have to set 'wcw.param' to the learner param which allows to set class weights! (which hopefully exists...)", learner$id)
    }
  } else {
    assertSubset(wcw.param, getParamIds(learner$par.set))
  }

  if (!missing(wcw.weight)) {
    assertNumeric(wcw.weight, lower = 0, any.missing = FALSE)
    pv$wcw.weight = wcw.weight
  }
  id = stri_paste("weightedclasses", learner$id, sep = ".")
  ps = makeParamSet(
    makeNumericVectorLearnerParam(id = "wcw.weight", len = NA_integer_, lower = 0)
  )
  x = makeBaseWrapper(id, learner$type, learner, package = learner$package, par.set = ps, par.vals = pv,
    learner.subclass = "WeightedClassesWrapper", model.subclass = "WeightedClassesModel")
  x$wcw.param = wcw.param
  x
}

#' @export
trainLearner.WeightedClassesWrapper = function(.learner, .task, .subset = NULL, .weights, wcw.weight = 1, ...) {
  .task = subsetTask(.task, .subset)
  td = getTaskDesc(.task)
  levs = td$class.levels
  p = .learner$wcw.param
  if (length(levs) == 2L) {
    assertNumber(wcw.weight, lower = 0)
    wcw.weight = c(wcw.weight, 1)
    names(wcw.weight) = c(td$positive, td$negative)
  } else {
    assertNumeric(wcw.weight, len = length(levs), lower = 0)
    names(wcw.weight) = levs
  }
  if (is.null(p)) {
    y = as.character(getTaskTargets(.task))
    weights = wcw.weight[y]
    m = train(.learner$next.learner, task = .task, weights = weights)
  } else {
    .learner = setHyperPars(.learner, par.vals = setNames(list(wcw.weight), p))
    m = train(.learner$next.learner, task = .task)
  }
  makeChainModel(next.model = m, cl = "WeightedClassesModel")
}

#' @export
getLearnerProperties.WeightedClassesWrapper = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), "weights")
}
