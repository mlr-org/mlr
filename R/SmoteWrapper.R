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
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param sw.rate [\code{numeric(1)}]\cr
#'   Factor to oversample the smaller class. Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param sw.nn [\code{integer(1)}]\cr
#'   Number of nearest neighbors to consider.
#'   Default is 5.
#' @template ret_learner
#' @export
makeSmoteWrapper = function(learner, sw.rate, sw.nn = 5L) {
  learner = checkLearner(learner, "classif")
  checkArg(sw.rate, "numeric", len = 1L, na.ok = FALSE, lower = 1)
  sw.nn = convertInteger(sw.nn)
  checkArg(sw.nn, "integer", len = 1L, na.ok = FALSE, lower = 1L)

  id = paste(learner$id, "undersampled", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "sw.rate", lower = 1),
    makeIntegerLearnerParam(id = "sw.nn", lower = 1L)
  )
  pv = list(sw.rate = sw.rate)
  makeBaseWrapper(id, learner, package = "mlr", par.set = ps, par.vals = pv, cl = "SmoteWrapper")
}

#' @export
trainLearner.SmoteWrapper = function(.learner, .task, .subset, .weights = NULL, sw.rate, ...) {
  .task = subsetTask(.task, .subset)
  .task = smote(.task, rate = sw.rate)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "SmoteModel")
}


