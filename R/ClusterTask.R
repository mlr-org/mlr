#' @title Create a clustering task.
#'
#' @description
#' The task encapsulates the data and creates a clustering task.
#' It also contains a description object detailing further aspects of the data.
#'
#' Useful operators are:
#' \code{\link{getTaskType}},
#' \code{\link{getTaskFeatureNames}},
#' \code{\link{getTaskNFeats}},
#' \code{\link{getTaskSize}},
#' \code{\link{getTaskData}}
#' \code{\link{getTaskDesc}}, and
#' \code{\link{subsetTask}}.
#'
#' Object members:
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored.
#'   Use \code{\link{getTaskData}} in order to access it.}
#' \item{weights [\code{numeric}]}{See argument. \code{NULL} if not present.}
#' \item{blocking [\code{factor}]}{See argument. \code{NULL} if not present.}
#' \item{task.desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
#' }
#'
#' @template arg_id
#' @template arg_data_features_only
#' @template arg_weights
#' @template arg_blocking
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @examples
#'   makeClusterTask(data = iris[, -5L])
#' @export
#' @family task
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeUnsupervisedTask("cluster", data, weights, blocking, fixup.data, check.data)
  task$task.desc = makeClusterTaskDesc(id, data, weights, blocking)
  addClasses(task, "ClusterTask")
}

makeClusterTaskDesc = function(id, data, weights, blocking) {
  target = character(0L)
  td = makeTaskDescInternal("cluster", id, data, target, weights, blocking)
  return(addClasses(td, c("ClusterTaskDesc", "UnsupervisedTaskDesc")))
}

#' @export
print.ClusterTask = function(x, ...) {
  print.UnsupervisedTask(x)
}
