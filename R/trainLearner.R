#' Train an R learner.
#'
#' Mainly for internal use. Trains a wrapped learner on a given training set.
#' You have to implement this method if you want to add another learner to this package.
#'
#' Your implementation must adhere to the following:
#' The model must be fitted on the subset of \code{.task} given by \code{.subset}. All parameters
#' in \code{...} must be passed to the underlying training function.
#'
#' @param .learner [\code{\link{RLearner}}]\cr
#'   Wrapped learner.
#' @param .task [\code{\link{Task}}]\cr
#'   Task to train learner on.
#' @param .subset [\code{integer}]\cr
#'   Subset of cases for training set, index the task with this.
#'   You probably want to use \code{\link{getTaskData}} for this purpose.
#' @param .weights [\code{numeric}]\cr
#'   Weights for each observation.
#' @param ... [any]\cr
#'   Additional (hyper)parameters, which need to be passed to the underlying train function.
#' @return [any]. Model of the underlying learner.
#' @export
trainLearner = function(.learner, .task, .subset, .weights = NULL, ...) {
  UseMethod("trainLearner")
}

