# FIXME: probs can only be predicted for two class problems (winning class)
#' @export
makeRLearner.classif.fnn = function() {
  makeRLearnerClassif(
    cl = "classif.fnn",
    package = "FNN",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeLogicalLearnerParam(id = "prob", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "algorithm", default = "cover_tree",
        values = list("cover_tree", "kd_tree", "brute"))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Fast k-Nearest Neighbour",
    short.name = "fnn",
    callees = "knn"
  )
}

#' @export
trainLearner.classif.fnn = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  list(train = d, parset = list(...))
}

#' @export
predictLearner.classif.fnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  pars = list(train = m$train$data, test = .newdata, cl = m$train$target)
  pars = c(pars, m$parset, list(...))
  p = do.call(FNN::knn, pars)
  attr(p, "nn.index") = NULL
  attr(p, "nn.dist") = NULL
  return(p)
}
