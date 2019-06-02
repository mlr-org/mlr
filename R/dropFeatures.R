#' Drop some features of task.
#'
#' @template arg_task
#' @param features ([character])\cr
#'   Features to drop.
#' @template ret_task
#' @export
#' @family eda_and_preprocess
dropFeatures = function(task, features) {
  assertClass(task, classes = "Task")
  f = getTaskFeatureNames(task)
  assertSubset(features, choices = f)
  subsetTask(task, features = setdiff(f, features))
}
