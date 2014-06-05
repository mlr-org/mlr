#' Drop some features of task.
#'
#' @template arg_task
#' @param features [\code{character}]\cr
#'   Features to drop.
#' @template ret_task
#' @export
dropFeatures = function(task, features) {
  checkArg(task, "SupervisedTask")
  f = getTaskFeatureNames(task)
  checkArg(features, subset = f)
  subsetTask(task, features = setdiff(f, features))
}


