#' @title Create a regression task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#'
#' Useful operators are: \code{\link{getTaskFormula}},
#' \code{\link{getTaskFeatureNames}},
#' \code{\link{getTaskData}},
#' \code{\link{getTaskTargets}}, and
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
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
#' @param target [\code{character(1)}]\cr
#'   Name(s) of the target variable(s).
#' @template arg_weights
#' @template arg_blocking
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @examples
#' if (requireNamespace("mlbench")) {
#'   library(mlbench)
#'   data(BostonHousing)
#'   makeRegrTask(data = BostonHousing, target = "medv")
#' }
#' @export
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("regr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeRegrTaskDesc(id, data, target, weights, blocking)
  addClasses(task, "RegrTask")
}

makeRegrTaskDesc = function(id, data, target, weights, blocking) {
  addClasses(makeTaskDescInternal("regr", id, data, target, weights, blocking), c("RegrTaskDesc", "SupervisedTaskDesc"))
}
