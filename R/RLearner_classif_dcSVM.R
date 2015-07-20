#' @export
makeRLearner.classif.dcSVM = function() {
  makeRLearnerClassif(
    cl = "classif.dcSVM",
    package = "SwarmSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 4, lower = 1),
      makeIntegerLearnerParam(id = "m", default = 800, lower = 1),
      makeDiscreteLearnerParam(id = "kernel", default = 3, values = c(1,2,3)),
      makeIntegerLearnerParam(id = "max.levels", default = 1, lower = 1),
      makeIntegerLearnerParam(id = "early", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "final.training", default = FALSE),
      makeLogicalLearnerParam(id = "pre.scale", default = FALSE),
      makeNumericLearnerParam(id = "seed"),
      makeLogicalLearnerParam(id = "verbose", default = TRUE),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeDiscreteLearnerParam(id = "cluster.method", default = "kmeans", values = c("kmeans", "kernkmeans")),
      makeFunctionLearnerParam(id = "cluster.fun"),
      makeFunctionLearnerParam(id = "cluster.predict")
    ),
    properties = c("twoclass", "numerics"),
    name = "Divided-Conquer Support Vector Machines",
    short.name = "dcSVM",
    note = "m set to 800 and max.levels set to 1 by default"
  )
}

#' @export
trainLearner.classif.dcSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  pars = list(...)
  m = NULL
  max.levels = NULL
  k = NULL
  if (!any(grepl('m', names(pars)))) {
    m = 800
  } else {
    m = pars$m
  }
  if (!any(grepl('max.levels', names(pars)))) {
    max.levels = 1
  } else {
    max.levels = pars$max.levels
  }
  if (!any(grepl('max.levels', names(pars)))) {
    k = 4
  } else { 
    k = pars$k
  }
  m = min(nrow(d$data), m)
  min.cluster = ceiling(5*m/(k^max.levels))
  if (min.cluster>m) {
    f = getTaskFormula(.task)
    result = e1071::svm(f, data = getTaskData(.task, .subset), probability = FALSE, ...)
    return(result)
  }

  
  if (!is.null(m) && !is.null(max.levels)) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, m = m, max.levels = max.levels, ...)
  } else if (is.null(m) && !is.null(max.levels)) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, max.levels = max.levels, ...)
  } else if (!is.null(m) && is.null(max.levels)) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, m = m, ...)
  } else {
    SwarmSVM::dcSVM(x = d$data, y = d$target, ...)
  }
}

#' @export
predictLearner.classif.dcSVM = function(.learner, .model, .newdata, ...) {
  factor(predict(.model$learner.model, newdata = .newdata, ...), 
    labels = .model$factor.levels$Class)
}
