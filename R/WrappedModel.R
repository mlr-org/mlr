#' @title Induced model of learner.
#'
#' @description
#' Result from \code{\link{train}}.
#'
#' It internally stores the underlying fitted model,
#' the subset used for training, features used for training, levels of factors in the
#' data set and computation time that was spent for training.
#'
#' Object members: See arguments.
#'
#' The constructor \code{makeWrappedModel} is mainly for internal use.
#'
#' @template arg_learner
#' @param learner.model [any]\cr
#'   Underlying model.
#' @template arg_taskdesc
#' @param subset [\code{integer}]\cr
#'   Subset used for training.
#' @param features [\code{character}]\cr
#'   Features used for training.
#' @param factor.levels [named \code{list} of \code{character}]\cr
#'   Levels of factor variables (features and potentially target) in training data.
#'   Named by variable name, non-factors do not occur in the list.
#' @param time [\code{numeric(1)}]\cr
#'   Computation time for model fit in seconds.
#'  @template ret_wmodel
#' @export
#' @aliases WrappedModel
makeWrappedModel = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  UseMethod("makeWrappedModel")
}

#' @export
makeWrappedModel.Learner = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  if(is.error(learner.model)) {
    learner.model = as.character(learner.model)
    time = NA_real_
    cl = c("FailureModel", "WrappedModel")
  } else {
    cl = "WrappedModel"
  }
  makeS3Obj(cl,
    learner = learner,
    learner.model = learner.model,
    task.desc = task.desc,
    subset = subset,
    features = features,
    factor.levels = factor.levels,
    time = time
  )
}

#' Get underlying R model of learner integrated into mlr.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   The model, returned by e.g., \code{\link{train}}.
#' @return [any]. A fitted model, depending the learner / wrapped package. E.g., a
#'   model of class \code{\link[rpart]{rpart}} for learner \dQuote{classif.rpart}.
#' @export
getLearnerModel = function(model) {
  UseMethod("getLearnerModel")
}

#'@export
getLearnerModel.WrappedModel = function(model) {
  model$learner.model
}

#' @export
print.WrappedModel = function(x, ...) {
  cat(
    "Model for id = ", x$learner$id, " class = ", getClass1(x$learner), "\n",
    "Trained on obs: ", length(x$subset), "\n",
    "Used features: ", length(x$features), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner), "\n",
    sep = ""
  )
}
