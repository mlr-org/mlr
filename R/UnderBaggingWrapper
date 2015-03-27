#' @title Fuse learner with the bagging technique and undersampling for imbalancy correction.
#'
#' @description
#' Fuses a classification learner for binary classification with an under-bagging method
#' for imbalancy correction when we have strongly unequal class sizes.
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getHomogeneousEnsembleModels}}.
#'
#' UnderBagging is implemented as follows:
#' For each iteration a random data subset is sampled. Majority class examples
#' are undersampled with a given rate.
#' Minority class examples are either simply copied into each bag, or bootstrapped with replacement
#' until we have as many minority class examples as in the original training data.
#' Features are currently not changed or sampled.
#'
#' Prediction works as follows:
#' For classification we do majority voting to create a discrete label and
#' probabilities are predicted by considering the proportions of all predicted labels.
#'
#' @template arg_learner
#' @param ubw.iters [\code{integer(1)}]\cr
#'   Number of fitted models in bagging.
#'   Default is 10.
#' @param ubw.rate [\code{numeric(1)}]\cr
#'   Factor to downsample the larger class in each bag.
#'   Must be > 0 and  <= 1,
#'   where 1 means no undersampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   Default is 1.
#' @param ubw.maxcl [\code{character(1)}]\cr
#'   character value that controls how to sample minority class.
#'   \dQuote{all} means every instance of the minority class gets in each bag,
#'   \dQuote{boot} means the minority class instances are bootstrapped in each iteration.
#'   Default is \dQuote{boot}.
#' @template ret_learner
#' @family imbalancy
#' @family wrapper
#' @export
makeUnderBaggingWrapper = function(learner, ubw.iters = 10L, ubw.rate = 1, ubw.maxcl = "boot") {
  
  learner = checkLearner(learner, "classif")
  pv = list()
  if (!missing(ubw.iters)) {
    ubw.iters = asCount(ubw.iters, positive = TRUE)
    pv$ubw.iters = ubw.iters
  }
  if (!missing(ubw.rate)) {
    assertNumber(ubw.rate, lower = 0, upper = 1)
    pv$ubw.rate = ubw.rate
  }
  if (!missing(ubw.maxcl)) {
    assertChoice(ubw.maxcl, choices = c("boot", "all"))
    pv$ubw.maxcl = ubw.maxcl
  }
  
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be response.")
  id = paste(learner$id, "underbagged", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "ubw.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "ubw.rate"),
    makeDiscreteLearnerParam(id = "ubw.maxcl", c("boot", "all"))
  )
  x = makeHomogeneousEnsemble(id, "classif", learner, packs, par.set = ps, par.vals = pv,
                              learner.subclass = c("UnderBaggingWrapper", "BaggingWrapper"), model.subclass = "BaggingModel")
  addProperties(x, "prob")
}

#' @export
trainLearner.UnderBaggingWrapper = function(.learner, .task, .subset, .weights = NULL,
                                           ubw.iters = 10L, ubw.rate = 1, ubw.maxcl = "boot", ...) {
  
  .task = subsetTask(.task, subset = .subset)
  y = getTaskTargets(.task)
  models = lapply(seq_len(ubw.iters), function(i) {
    bag = sampleBinaryClass(y, ubw.rate, cl = "max", clreplace = FALSE,
                            othreplace = (ubw.maxcl == "boot"), bagging = TRUE)
    train(.learner$next.learner, .task, subset = bag, weights = .weights)
  })
  m = makeHomChainModel(.learner, models)
}




