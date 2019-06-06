#' @title Fuse learner with SMOTE oversampling for imbalancy correction in binary classification.
#'
#' @description
#' Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses [smote] before every model fit.
#'
#' Note that observation weights do not influence the sampling and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @param sw.rate (`numeric(1)`)\cr
#'   Factor to oversample the smaller class. Must be between 1 and `Inf`,
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#'   Default is 1.
#' @param sw.nn (`integer(1)`)\cr
#'   Number of nearest neighbors to consider.
#'   Default is 5.
#' @param sw.standardize (`logical(1)`)\cr
#'   Standardize input variables before calculating the nearest neighbors
#'   for data sets with numeric input variables only. For mixed variables
#'   (numeric and factor) the gower distance is used and variables are
#'   standardized anyway.
#'   Default is `TRUE`.
#' @param sw.alt.logic (`logical(1)`)\cr
#'   Use an alternative logic for selection of minority class observations.
#'   Instead of sampling a minority class element AND one of its nearest
#'   neighbors, each minority class element is taken multiple times (depending
#'   on rate) for the interpolation and only the corresponding nearest neighbor
#'   is sampled.
#'   Default is `FALSE`.
#' @template ret_learner
#' @family wrapper
#' @export
makeSMOTEWrapper = function(learner, sw.rate = 1, sw.nn = 5L,
  sw.standardize = TRUE, sw.alt.logic = FALSE) {

  learner = checkLearner(learner, "classif")
  pv = list()
  if (!missing(sw.rate)) {
    assertNumber(sw.rate, lower = 1)
    pv$sw.rate = sw.rate
  }
  if (!missing(sw.nn)) {
    sw.nn = asCount(sw.nn, positive = TRUE)
    pv$sw.nn = sw.nn
  }
  if (!missing(sw.standardize)) {
    pv$sw.standardize = sw.standardize
  }
  if (!missing(sw.alt.logic)) {
    pv$sw.alt.logic = sw.alt.logic
  }
  id = stri_paste(learner$id, "smoted", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "sw.rate", lower = 1),
    makeIntegerLearnerParam(id = "sw.nn", lower = 1L),
    makeLogicalLearnerParam(id = "sw.standardize"),
    makeLogicalLearnerParam(id = "sw.alt.logic")
  )
  makeBaseWrapper(id, "classif", learner, package = learner$package, par.set = ps, par.vals = pv,
    learner.subclass = "SMOTEWrapper", model.subclass = "SMOTEModel")
}

#' @export
trainLearner.SMOTEWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, sw.rate = 1,
  sw.nn = 5, sw.standardize = TRUE, sw.alt.logic = FALSE, ...) {
  .task = subsetTask(.task, .subset)
  .task = smote(.task, rate = sw.rate, nn = sw.nn, standardize = sw.standardize, alt.logic = sw.alt.logic)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "SMOTEModel")
}
