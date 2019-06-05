#' Train an R learner.
#'
#' Mainly for internal use. Trains a wrapped learner on a given training set.
#' You have to implement this method if you want to add another learner to this package.
#'
#' Your implementation must adhere to the following:
#' The model must be fitted on the subset of `.task` given by `.subset`. All parameters
#' in `...` must be passed to the underlying training function.
#'
#' @param .learner ([RLearner])\cr
#'   Wrapped learner.
#' @param .task ([Task])\cr
#'   Task to train learner on.
#' @param .subset ([integer])\cr
#'   Subset of cases for training set, index the task with this.
#'   You probably want to use [getTaskData] for this purpose.
#' @param .weights ([numeric])\cr
#'   Weights for each observation.
#' @param ... (any)\cr
#'   Additional (hyper)parameters, which need to be passed to the underlying train function.
#' @return (any). Model of the underlying learner.
#' @export
trainLearner = function(.learner, .task, .subset, .weights = NULL, ...) {
  UseMethod("trainLearner")
}
