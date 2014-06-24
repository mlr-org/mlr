#' @title Fuse learner with the bagging technique and oversampling for imbalancy correction.
#'
#' @description
#' Fuses a classification learner for binary classification with an over-bagging method
#' for imbalancy correction when we have strongly unequal class sizes.
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getBaggingModels}}.
#'
#' OverBagging is implemented as follows:
#' For each iteration a random data subset is sampled. Minority class examples
#' are oversampled with replacement with a given rate.
#' Majority class examples are either simply copied into each bag, or bootstrapped with replacement
#' until we have as many majority class examples as in the original training data.
#' Features are currently not changed or sampled.
#'
#' Prediction works as follows:
#' For classification we do majority voting to create a discrete label and
#' probabilities are predicted by considering the proportions of all predicted labels.
#'
#' @template arg_learner
#' @param obw.iters [\code{integer(1)}]\cr
#'   Number of fitted models in bagging.
#'   Default is 10.
#' @param obw.rate [\code{numeric(1)}]\cr
#'   Factor to upsample the smaller class in each bag.
#'   Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param obw.maxcl [\code{character(1)}]\cr
#'   character value that controls how to sample majority class.
#'   \dQuote{all} means every instance of the majority class gets in each bag,
#'   \dQuote{boot} means the majority class instances are bootstrapped in each iteration.
#'   Default is \dQuote{boot}.
#' @template ret_learner
#' @family imbalancy
#' @export
makeOverBaggingWrapper = function(learner, obw.iters = 10L, obw.rate, obw.maxcl = "boot") {

  learner = checkLearner(learner, "classif")
  obw.iters = convertInteger(obw.iters)
  checkArg(obw.iters, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  checkArg(obw.rate, "numeric", len = 1L, na.ok = FALSE, lower = 1)
  assertChoice(obw.maxcl, choices = c("boot", "all"))

  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be response.")
  id = paste(learner$id, "overbagged", sep = ".")
  packs = learner$packages
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "obw.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "obw.rate", lower = 1),
    makeDiscreteLearnerParam(id = "obw.maxcl", c("boot", "all"))
  )
  pv = list(obw.iters = obw.iters, obw.rate = obw.rate, obw.maxcl = obw.maxcl)
  x = makeBaseWrapper(id, learner, packs, par.set = ps, par.vals = pv, cl = "OverBaggingWrapper")
  x = addProperties(x, "prob")

  return(x)
}

#' @export
trainLearner.OverBaggingWrapper = function(.learner, .task, .subset, .weights = NULL,
   obw.iters, obw.rate, obw.maxcl, ...) {

  .task = subsetTask(.task, subset = .subset)
  y = getTaskTargets(.task)
  models = lapply(seq_len(obw.iters), function(i) {
    bag = sampleBinaryClass(y, obw.rate, cl = "min", minreplace = TRUE, maxreplace = (obw.maxcl == "boot"))
    train(.learner$next.learner, .task, subset = bag, weights = .weights)
  })
  makeChainModel(next.model = models, cl = "OverBaggingModel")
}

#' @export
predictLearner.OverBaggingWrapper = function(.learner, .model, .newdata, ...) {
  models = getBaggingModels(.model)
  p = sapply(models, function(m) {
    as.character(predict(m, newdata = .newdata, ...)$data$response)
  })
  if (.learner$predict.type == "response") {
    g = as.factor(apply(p, 1L, computeMode))
  } else {
    levs = .model$task.desc$class.levels
    p = apply(p, 1L, function(x) {
      x = factor(x, levels = levs) # we need all level for the table and we need them in consitent order!
      as.numeric(prop.table(table(x)))
    })
    setColNames(t(p), levs)
  }
}

#' @export
makeWrappedModel.OverBaggingWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "OverBaggingModel")
}

#' @export
print.OverBaggingModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("OverBagged Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}
