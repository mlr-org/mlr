#' @export
makeRLearner.classif.dcSVM = function() {
  makeRLearnerClassif(
    cl = "classif.dcSVM",
    package = c("SwarmSVM", "e1071"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 4, lower = 1),
      makeIntegerLearnerParam(id = "m", lower = 1),
      makeDiscreteLearnerParam(id = "kernel", default = 3, values = c(1, 2, 3)),
      makeIntegerLearnerParam(id = "max.levels", lower = 1),
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
    callees = c("dcSVM", "svm")
  )
}

#' @export
trainLearner.classif.dcSVM = function(.learner, .task, .subset, .weights = NULL, ...) {

  d = getTaskData(.task, .subset, target.extra = TRUE)
  pars = list(...)
  m.flag = FALSE
  max.levels.flag = FALSE
  if (!any(stri_detect_regex(names(pars), "m"))) {
    m = 800
    m.flag = TRUE
  } else {
    m = pars$m
  }
  if (!any(stri_detect_regex(names(pars), "max.levels"))) {
    max.levels = 1
    max.levels.flag = TRUE
  } else {
    max.levels = pars$max.levels
  }
  if (!any(stri_detect_regex(names(pars), "k"))) {
    k = 4
  } else {
    k = pars$k
  }
  m = min(nrow(d$data), m)
  min.cluster = ceiling(5 * m / (k^max.levels))
  if (min.cluster > m) {
    f = getTaskFormula(.task)
    # map kernel to corresponding e1071 kernel
    if (!is.null(pars$kernel)) {
      kernel = c("linear", "polynomial", "radial")[pars$kernel]
    } else {
      kernel = "radial"
    }
    pars$kernel = kernel
    result = do.call(e1071::svm, c(f, list(data = getTaskData(.task, .subset), probability = FALSE), pars))
    return(result)
  }


  if (m.flag && max.levels.flag) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, m = m, max.levels = max.levels, ...)
  } else if (!m.flag && max.levels.flag) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, max.levels = max.levels, ...)
  } else if (m.flag && !max.levels.flag) {
    SwarmSVM::dcSVM(x = d$data, y = d$target, m = m, ...)
  } else {
    SwarmSVM::dcSVM(x = d$data, y = d$target, ...)
  }
}

#' @export
predictLearner.classif.dcSVM = function(.learner, .model, .newdata, ...) {
  prediction = predict(.model$learner.model, newdata = .newdata, ...)
  if (!is.factor(prediction)) { # depends on parameters AND data
    prediction = factor(prediction, levels = c(1, 2), labels = .model$factor.levels[[1]])
  }
  prediction
}
