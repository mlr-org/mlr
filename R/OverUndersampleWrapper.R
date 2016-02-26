#' @title Fuse learner with simple ove/underrsampling for imbalancy correction in binary classification.
#'
#' @description
#' Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses \code{\link{oversample}} or \code{\link{undersample}} before every model fit.
#'
#' Note that observation weights do not influence the sampling and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @param usw.rate [\code{numeric(1)}]\cr
#'   Factor to downsample a class. Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   Default is 1.
#' @param osw.rate [\code{numeric(1)}]\cr
#'   Factor to oversample a class. Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#'   Default is 1.
#' @param usw.cl [\code{character(1)}]\cr
#'   Class that should be undersampled.
#'   Default is \code{NULL}, which means the larger one.
#' @param osw.cl [\code{character(1)}]\cr
#'   Class that should be oversampled.
#'   Default is \code{NULL}, which means the smaller one.
#' @template ret_learner
#' @family imbalancy
#' @family wrapper
#' @export
makeUndersampleWrapper = function(learner, usw.rate = 1, usw.cl = NULL) {
  learner = checkLearner(learner, "classif")
  pv = list()
  if (!missing(usw.rate)) {
    assertNumber(usw.rate, lower = 0, upper = 1)
    pv$usw.rate = usw.rate
  }
  if (!is.null(usw.cl)) {
    assertString(usw.cl)
    pv$usw.cl = usw.cl
  }
  id = paste(learner$id, "undersampled", sep = ".")
  ps = makeParamSet(
    makeNumericLearnerParam(id = "usw.rate", lower = 0, upper = 1),
    makeUntypedLearnerParam(id = "usw.cl", default = NULL)
  )
  makeBaseWrapper(id, "classif", learner, package = "mlr", par.set = ps, par.vals = pv,
    learner.subclass = "UndersampleWrapper", model.subclass = "UndersampleModel")
}

#' @rdname makeUndersampleWrapper
#' @export
makeOversampleWrapper = function(learner, osw.rate = 1, osw.cl = NULL) {
  learner = checkLearner(learner, "classif")
  pv = list()
  if (!missing(osw.rate)) {
    assertNumber(osw.rate, lower = 1)
    pv$osw.rate = osw.rate
  }
  if (!is.null(osw.cl)) {
    assertString(osw.cl)
    pv$osw.cl = osw.cl
  }
  id = paste(learner$id, "oversampled", sep = ".")
  ps = makeParamSet (
    makeNumericLearnerParam(id = "osw.rate", lower = 1),
    makeUntypedLearnerParam(id = "osw.cl", default = NULL)
  )
  makeBaseWrapper(id, "classif", learner, package = "mlr", par.set = ps, par.vals = pv,
    learner.subclass = "OversampleWrapper", model.subclass = "OversampleModel")
}

#' @export
trainLearner.UndersampleWrapper = function(.learner, .task, .subset, .weights = NULL, usw.rate = 1, usw.cl = NULL, ...) {
  .task = subsetTask(.task, .subset)
  .task = undersample(.task, rate = usw.rate, cl = usw.cl)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "UndersampleModel")
}

#' @export
trainLearner.OversampleWrapper = function(.learner, .task, .subset, .weights = NULL, osw.rate = 1, osw.cl = NULL, ...) {
  .task = subsetTask(.task, .subset)
  .task = oversample(.task, rate = osw.rate, cl = osw.cl)
  m = train(.learner$next.learner, .task, weights = .weights)
  makeChainModel(next.model = m, cl = "OversampleModel")
}

