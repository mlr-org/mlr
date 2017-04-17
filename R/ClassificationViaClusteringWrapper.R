#' @title Classification via clustering wrapper.
#'
#' @description
#' Clusters the training data and determines the distribution of class labels for each cluster. New data is assigned to a cluster and the class distribution (or label) for this cluster is returned as the prediction.
#'
#' Fuzzy cluster memberships are currently not used.
#'
#' @template arg_learner
#' @param predict.type [\code{character(1)}]\cr
#'   \dQuote{response} (= labels) or \dQuote{prob} (= probabilities and labels by selecting the ones with maximal probability).
#' @param ... [any]\cr
#'   Additional parameters passed down to the filter.
#' @template ret_learner
#' @export
#' @family wrapper
#' @examples
#' lrn = makeLearner("cluster.EM")
#' lrn = makeClassificationViaClusteringWrapper(lrn)
#' mod = train(lrn, iris.task, subset = 1:100)
#' predictions = predict(mod, newdata = iris[101:150, 1:4])
makeClassificationViaClusteringWrapper = function(learner, predict.type = "response", ...) {
  learner = checkLearner(learner, "cluster")
  ddd = list(...)
  assertList(ddd, names = "named")

  lrn = makeLearnerBaseConstructor(classes = c("ClassificationViaClusteringWrapper", "Wrapper"),
    id = paste(learner$id, "as.classify", sep = "."),
    type = "classif",
    predict.type = predict.type,
    properties = c(learner$properties, "oneclass", "twoclass", "multiclass"),
    package = learner$package,
    par.set = list(),
    par.vals = list()
  )
  lrn$model.subclass = "ClassificationViaClusteringModel"
  lrn$next.learner = learner
  lrn$fix.factors.prediction = FALSE
  lrn$more.args = ddd
  lrn
}

#' @export
trainLearner.ClassificationViaClusteringWrapper = function(.learner, .task, .subset, .weights = NULL, ... ) {
  cluster.task = makeClusterTask(id = getTaskId(.task),
    data = getTaskData(.task)[, getTaskFeatureNames(.task), drop = FALSE],
    weights = .task$weights,
    blocking = .task$blocking)
  m = train(.learner$next.learner, cluster.task, .subset, weights = .weights)
  cm = makeChainModel(next.model = m, cl = "ClassificationViaClusteringModel")

  # assign class counts to clusters
  cluster.assignments = predict(m, task = cluster.task, subset = .subset)
  cm$class.distributions = lapply(unique(getResponse(cluster.assignments)),
    function(ci) {
      tab = table(factor(getTaskTargets(.task)[getResponse(cluster.assignments) == ci], levels = .task$task.desc$class.levels))
      t(as.matrix(tab))
    }
  )
  return(cm)
}


#' @export
predictLearner.ClassificationViaClusteringWrapper = function(.learner, .model, .newdata, ...) {
  clusters = do.call(predictLearner, c(
    list(.learner = .learner$next.learner, .model = getLearnerModel(.model)$next.model, .newdata = .newdata), ...)
  )

  fun = switch(.learner$predict.type,
    response = function(ci) {
      dist = getLearnerModel(.model)$class.distributions[[ci]]
      colnames(dist)[which.max(dist)]
    },
    prob = function(ci) {
      dist = getLearnerModel(.model)$class.distributions[[ci]]
      total = sum(dist)
      dist/total
    }
  )

  if (.learner$predict.type == "response") {
    factor(sapply(clusters, fun), levels = .model$task.desc$class.levels)
  } else {
    do.call(rbind, lapply(clusters, fun))
  }
}
