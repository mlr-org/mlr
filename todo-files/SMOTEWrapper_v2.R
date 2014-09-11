#' @title Fuse learner with SMOTE oversampling for imbalancy correction in binary classification.
#'
#' @description
#' Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{smote}} before every model fit.
#'
#' Note that observation weights do not influence the sampling and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @param sw.rate [\code{numeric(1)}]\cr
#'   Factor to oversample the smaller class. Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#'   Default is 1.
#' @param sw.nn [\code{integer(1)}]\cr
#'   Number of nearest neighbors to consider.
#'   Default is 5.
#' @template ret_learner
#' @export
makeSMOTEWrapper = function(learner, sw.rate = 1, sw.nn = 5L, 
  sw.standardize = TRUE, sw.useAltLogic = FALSE) {
  
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
  if (!missing(sw.useAltLogic)) {
    pv$sw.useAltLogic = sw.useAltLogic
  }
  id = paste(learner$id, "smoted", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "sw.rate", lower = 1),
    makeIntegerLearnerParam(id = "sw.nn", lower = 1L),
    makeLogicalLearnerParam(id = "sw.standardize"),
    makeLogicalLearnerParam(id = "sw.useAltLogic")
  )
  makeBaseWrapper(id, learner, package = learner$package, par.set = ps, par.vals = pv, cl = "SMOTEWrapper")
}

#' @export
trainLearner.SMOTEWrapper = function(.learner, .task, .subset, .weights = NULL, sw.rate = 1,
  sw.standardize = FALSE, sw.useAltLogic = FALSE, ...) {
  
  .task = subsetTask(.task, .subset)
  .task = smote(.task, rate = sw.rate, standardize = sw.standardize, useAltLogic = sw.useAltLogic)  
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "SMOTEModel")
}
