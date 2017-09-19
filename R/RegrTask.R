#' @title Create a regression task.
#'
#' @template desc_tasks
#' @templateVar desc_tasks_tasktype regression
#' @templateVar desc_tasks_further_notes The target column must be of type numeric.
#'
#' @inheritParams makeTask
#' @template arg_id
#' @param target [\code{character(1)}]\cr
#'   Name(s) of the target variable(s).
#' @return [\code{\link{RegrTask}}].
#' @examples
#' if (requireNamespace("mlbench")) {
#'   library(mlbench)
#'   data(BostonHousing)
#'   makeRegrTask(data = BostonHousing, target = "medv")
#' }
#' @export
#' @family task
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
