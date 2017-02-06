#' @title Functional data regression task
#'
#' @description Create a functional data regression task. The data is assumed
#'   to have functional data structure and the target variable must be specified
#'   by the user.
#'
#' @param id
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Data with functional structure.
#' @param target [\code{character}]\cr
#'   Target variable name.
#' @param weights
#' @param blocking
#' @param positive
#' @param fixup.data
#' @param check.data
#' @param channel.list A list indicating different channels of the input
#'
#' @return Functional data regression task. Object of type
#'   \code{FDARegrTask}.
#'
#' @rdname Task
#' @export
makeFDARegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE, channel.list = NULL,
  formula.list = NULL, index.list = NULL) {

  task = makeSupervisedTask("regr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)
  task = makeRegrTask(id, data, target, weights, blocking, fixup.data, check.data)
  type = "tsregr"
  task$type = type
  task$task.desc$type = type
  task$channel.list = channel.list
  task$formula.list = formula.list
  task$task.desc = addClasses(task$task.desc, "TaskDescFDARegr")
  addClasses(task, "FDARegrTask")
}
