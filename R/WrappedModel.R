#' @title Induced model of learner.
#'
#' @description
#' Result from [train].
#'
#' It internally stores the underlying fitted model,
#' the subset used for training, features used for training, levels of factors in the
#' data set and computation time that was spent for training.
#'
#' Object members: See arguments.
#'
#' The constructor `makeWrappedModel` is mainly for internal use.
#'
#' @template arg_learner
#' @param learner.model (any)\cr
#'   Underlying model.
#' @template arg_taskdesc
#' @template arg_subset
#' @param features ([character])\cr
#'   Features used for training.
#' @param factor.levels (named [list] of [character])\cr
#'   Levels of factor variables (features and potentially target) in training data.
#'   Named by variable name, non-factors do not occur in the list.
#' @param time (`numeric(1)`)\cr
#'   Computation time for model fit in seconds.
#' @template ret_wmodel
#' @export
#' @aliases WrappedModel
makeWrappedModel = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  UseMethod("makeWrappedModel")
}

#' @export
makeWrappedModel.Learner = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  dump = NULL
  if (is.error(learner.model)) {
    learner.model = as.character(learner.model)
    time = NA_real_
    cl = c("FailureModel", "WrappedModel")
    if (getLearnerOptions(learner, "on.error.dump")$on.error.dump) {
      dump = addClasses(get("last.dump", envir = .GlobalEnv), "mlr.dump")
    }
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
    time = time,
    dump = dump
  )
}

#' @export
print.WrappedModel = function(x, ...) {
  cat(
    "Model for learner.id=", x$learner$id, "; learner.class=", getClass1(x$learner), "\n",
    sprintf("Trained on: task.id = %s; obs = %i; features = %i",
      x$task.desc$id, length(x$subset), length(x$features)), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner, show.missing.values = TRUE), "\n",
    sep = ""
  )
  if (isFailureModel(x)) {
    catf("Training failed: %s", getFailureModelMsg(x))
  }
}

#' Get underlying R model of learner integrated into mlr.
#'
#' @param model ([WrappedModel])\cr
#'   The model, returned by e.g., [train].
#' @param more.unwrap (`logical(1)`)\cr
#'   Some learners are not basic learners from R, but implemented in mlr as meta-techniques.
#'   Examples are everything that inherits from `HomogeneousEnsemble`.
#'   In these cases, the `learner.model` is often a list of mlr [WrappedModel]s.
#'   This option allows to strip them further to basic R models.
#'   The option is simply ignored for basic learner models.
#'   Default is `FALSE`.
#' @return (any). A fitted model, depending the learner / wrapped package. E.g., a
#'   model of class [rpart::rpart] for learner \dQuote{classif.rpart}.
#' @export
getLearnerModel = function(model, more.unwrap = FALSE) {
  assertFlag(more.unwrap)
  UseMethod("getLearnerModel")
}

#' @export
getLearnerModel.WrappedModel = function(model, more.unwrap) {
  model$learner.model
}

#' @title Is the model a FailureModel?
#'
#' @description
#' Such a model is created when one sets the corresponding option in [configureMlr].
#'
#' For complex wrappers this getter returns `TRUE` if ANY model contained in it failed.
#'
#' @template arg_wrappedmod
#' @return (`logical(1)`).
#' @export
isFailureModel = function(model) {
  UseMethod("isFailureModel")
}

#' @export
# by default the model is never a failure. if a failure happens we have the derived class FailureModel
isFailureModel.WrappedModel = function(model) {
  return(FALSE)
}

#' @title Return error message of FailureModel.
#'
#' @description
#' Such a model is created when one sets the corresponding option in [configureMlr].
#' If no failure occurred, `NA` is returned.
#'
#' For complex wrappers this getter returns the first error message encountered in ANY model that failed.
#'
#' @template arg_wrappedmod
#' @return (`character(1)`).
#' @export
getFailureModelMsg = function(model) {
  UseMethod("getFailureModelMsg")
}

#' @export
getFailureModelMsg.WrappedModel = function(model) {
  return(NA_character_)
}

#' @title Return the error dump of FailureModel.
#'
#' @description
#' Returns the error dump that can be used with `debugger()` to evaluate errors.
#' If [configureMlr] configuration `on.error.dump` is `FALSE`, this returns
#' `NULL`.
#'
#' @template arg_wrappedmod
#' @return (`last.dump`).
#' @export
getFailureModelDump = function(model) {
  UseMethod("getFailureModelDump")
}

#' @export
getFailureModelDump.WrappedModel = function(model) {
  return(NULL)
}
