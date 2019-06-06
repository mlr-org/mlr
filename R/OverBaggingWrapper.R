#' @title Fuse learner with the bagging technique and oversampling for imbalancy correction.
#'
#' @description
#' Fuses a classification learner for binary classification with an over-bagging method
#' for imbalancy correction when we have strongly unequal class sizes.
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via [getLearnerModel].
#'
#' OverBagging is implemented as follows:
#' For each iteration a random data subset is sampled. Class examples
#' are oversampled with replacement with a given rate.
#' Members of the other class are either simply copied into each bag, or bootstrapped with replacement
#' until we have as many majority class examples as in the original training data.
#' Features are currently not changed or sampled.
#'
#' Prediction works as follows:
#' For classification we do majority voting to create a discrete label and
#' probabilities are predicted by considering the proportions of all predicted labels.
#'
#' @template arg_learner
#' @param obw.iters (`integer(1)`)\cr
#'   Number of fitted models in bagging.
#'   Default is 10.
#' @param obw.rate (`numeric(1)`)\cr
#'   Factor to upsample a class in each bag.
#'   Must be between 1 and `Inf`,
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#'   Default is 1.
#' @param obw.maxcl (`character(1)`)\cr
#'   How should other class (usually larger class) be handled?
#'   \dQuote{all} means every instance of the class gets in each bag,
#'   \dQuote{boot} means the class instances are bootstrapped in each iteration.
#'   Default is \dQuote{boot}.
#' @param obw.cl (`character(1)`)\cr
#'   Which class should be over- or undersampled. If `NULL`, `makeOverBaggingWrapper`
#'   will take the smaller class.
#' @template ret_learner
#' @family imbalancy
#' @family wrapper
#' @export
makeOverBaggingWrapper = function(learner, obw.iters = 10L, obw.rate = 1, obw.maxcl = "boot", obw.cl = NULL) {

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
  if (!is.null(obw.cl)) {
    assertString(obw.cl)
    pv$obw.cl = obw.cl
  }

  if (learner$predict.type != "response") {
    stop("Predict type of the basic learner must be response.")
  }
  id = stri_paste(learner$id, "overbagged", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "obw.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "obw.rate", lower = 1),
    makeDiscreteLearnerParam(id = "obw.maxcl", c("boot", "all")),
    makeUntypedLearnerParam(id = "obw.cl", default = NULL, tunable = FALSE)
  )
  makeHomogeneousEnsemble(id, "classif", learner, packs, par.set = ps, par.vals = pv,
    learner.subclass = c("OverBaggingWrapper", "BaggingWrapper"), model.subclass = "BaggingModel")
}

#' @export
trainLearner.OverBaggingWrapper = function(.learner, .task, .subset = NULL, .weights = NULL,
  obw.iters = 10L, obw.rate = 1, obw.maxcl = "boot", obw.cl = NULL, ...) {

  .task = subsetTask(.task, subset = .subset)
  y = getTaskTargets(.task)
  if (is.null(obw.cl)) {
    z = getMinMaxClass(y)
    obw.cl = z$min.name
  }
  args = list("y" = y, "obw.rate" = obw.rate, "obw.maxcl" = obw.maxcl, "obw.cl" = obw.cl, "learner" = .learner, "task" = .task, "weights" = .weights)
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doOverBaggingTrainIteration, i = seq_len(obw.iters), more.args = args)
  makeHomChainModel(.learner, models)
}

doOverBaggingTrainIteration = function(i, y, obw.rate, obw.cl, obw.maxcl, learner, task, weights) {
  setSlaveOptions()
  bag = sampleBinaryClass(y, rate = obw.rate, cl = obw.cl, resample.other.class = (obw.maxcl == "boot"))
  train(learner$next.learner, task, subset = bag, weights = weights)
}


#' @export
getLearnerProperties.OverBaggingWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "prob")
}
