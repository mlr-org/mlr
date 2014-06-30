#' @title Wraps a classifier for weighted fitting where each class receives a weight.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#'
#' Fitting is performed in a weighted fashion where each observation receives a weight,
#' depending on the class is belongs to, see \code{wcw.weight}.
#' This might help to mitigate problems in imbalanced classification problems.
#'
#' @template arg_learner_classif
#' @param wcw.weight [\code{numeric}]\cr
#'   Weight for each class.
#'   Must be a vector of the same number of elements as classes are in task,
#'   and must also be in the same order as the class levels are in
#'   \code{task$task.desc$class.levels}.
#'   For convenience, one must pass a single number in case of binary classification, which
#'   is then taken as the weight of the positive class, while the negative class receives a weight
#'   of 1.
#' @template ret_learner
#' @export
makeWeightedClassesWrapper = function(learner, wcw.weight) {
  learner = checkLearnerClassif(learner, weights = TRUE)
  id = paste("weightedclasses", learner$id, sep = ".")
  ps = makeParamSet(
    makeNumericVectorLearnerParam(id = "wcw.weight", len = NA_integer_, lower = 0)
  )
  pv = list(wcw.weight = wcw.weight)
  x = makeBaseWrapper(id, learner, package = learner$package, par.set = ps, par.vals = pv,
    cl = "WeightedClassesWrapper")
  removeProperties(x, "weights")
}

#' @export
trainLearner.WeightedClassesWrapper = function(.learner, .task, .subset, .weights, wcw.weight, ...) {
  .task = subsetTask(.task, .subset)
  td = .task$task.desc
  levs = td$class.levels
  if (length(levs) == 2L) {
    assertNumber(wcw.weight, lower = 0)
    wcw.weight = c(wcw.weight, 1)
    names(wcw.weight) = c(td$positive, td$negative)
  } else {
    assertNumeric(wcw.weight, len = length(levs), lower = 0)
    names(wcw.weight) = levs
  }
  y = as.character(getTaskTargets(.task))
  weights = wcw.weight[y]
  m = train(.learner$next.learner, task = .task, weights = weights)
  makeChainModel(next.model = m, cl = "WeightedClassesModel")
}


