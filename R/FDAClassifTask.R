#' @title Functional data classification task
#'
#' @description Create a functional data classification task. The data is assumed
#'   to have functional data structure and the target variable must be specified
#'   by the user.
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Data with functional structure.
#' @param target [\code{character}]\cr
#'   Target variable name.

#' @param channel.list A list indicating different channels of the input
#'
#' @return Functional data classification task. Object of type
#'   \code{FunctionDataClassifTask}.
#'
#' @rdname Task
#' @export
makeFDAClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn",
  check.data = TRUE, channel.list = NULL) {

  task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)
  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  type = "fdaclassif"

  task$type = type
  task$task.desc$type = type
  task$channel.list = channel.list
  task$task.desc = addClasses(task$task.desc, "TaskDescFDAClassif")
  addClasses(task, "FDAClassifTask")
}
