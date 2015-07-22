#' @title Fuse learner with the bagging technique and oversampling for imbalancy correction.
#'
#' @description
#' Fuses a classification learner for binary classification with an over-bagging method
#' for imbalancy correction when we have strongly unequal class sizes.
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getLearnerModel}}.
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
#'   Default is 1.
#' @param obw.maxcl [\code{character(1)}]\cr
#'   character value that controls how to sample majority class.
#'   \dQuote{all} means every instance of the majority class gets in each bag,
#'   \dQuote{boot} means the majority class instances are bootstrapped in each iteration.
#'   Default is \dQuote{boot}.
#' @template ret_learner
#' @family imbalancy
#' @family wrapper
#' @export
makeOverBaggingWrapper = function(learner, obw.iters = 10L, obw.rate = 1, obw.maxcl = "boot") {

  learner = checkLearner(learner, "classif")
  pv = list()
  if (!missing(obw.iters)) {
    obw.iters = asCount(obw.iters, positive = TRUE)
    pv$obw.iters = obw.iters
  }
  if (!missing(obw.rate)) {
    assertNumber(obw.rate, lower = 1)
    pv$obw.rate = obw.rate
  }
  if (!missing(obw.maxcl)) {
    assertChoice(obw.maxcl, choices = c("boot", "all"))
    pv$obw.maxcl = obw.maxcl
  }

  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be response.")
  id = paste(learner$id, "overbagged", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "obw.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "obw.rate", lower = 1),
    makeDiscreteLearnerParam(id = "obw.maxcl", c("boot", "all"))
  )
  makeHomogeneousEnsemble(id, "classif", learner, packs, par.set = ps, par.vals = pv,
     learner.subclass = c("OverBaggingWrapper", "BaggingWrapper"), model.subclass = "BaggingModel")
}

#' @export
trainLearner.OverBaggingWrapper = function(.learner, .task, .subset, .weights = NULL,
   obw.iters = 10L, obw.rate = 1, obw.maxcl = "boot", ...) {

  .task = subsetTask(.task, subset = .subset)
  y = getTaskTargets(.task)
  models = lapply(seq_len(obw.iters), function(i) {
    bag = sampleBinaryClass(y, obw.rate, cl = "min", clreplace = TRUE,
      othreplace = (obw.maxcl == "boot"), bagging = TRUE)
    train(.learner$next.learner, .task, subset = bag, weights = .weights)
  })
  m = makeHomChainModel(.learner, models)
}

#' @export
getLearnerProperties.OverBaggingWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "prob")
}
