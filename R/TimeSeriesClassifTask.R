#' @title Time Series classification task
#'
#' @description Create a temporal data classification task. The data is assumed
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
#' @param channel.list
#'
#' @return Time series classification task. Object of type
#'   \code{TimeSeriesClassifTask}.
#'
#' @rdname Task
#' @export
makeTimeSeriesClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn",
  check.data = TRUE, channel.list = NULL) {

  task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)
  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  type = "tsclassif"

  task$type = type
  task$task.desc$type = type
  task$channel.list = channel.list
  task$task.desc = addClasses(task$task.desc, "TaskDescTimeSeriesClassif")
  addClasses(task, "TimeSeriesClassifTask")
}

#' #' @export
#' #' @rdname Task
#' #FIXME: currently the following will not work, since mlr does not support put meta information on the column, which could be a solution to seperate each channel or modal
#' makeTimeSeriesMultiChannelClassifTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE, channel.list) {
#'   checkmate::assert_list(channel.list)
#'   task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)
#'   task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
#'   task = makeTimeSeriesClassifTask(id, data, target, weights, blocking, positive, fixup.data , check.data)
#'   type = "ts_mc_classif" #multi channel
#'   task$type = type
#'   task$task.desc$type = type
#'   task$task.desc = addClasses(task$task.desc, "TaskDescTimeSeriesMultiChannelClassif")
#'   task$channel.list = channel.list
#'   addClasses(task, "TimeSeriesMultiChannelClassifTask")
#' }
