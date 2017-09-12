#' @title Create a cost-sensitive classification task.
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
#' @param id [\code{character(1)}]\cr
#'   Id string for object.
#'   Default is the name of the R variable passed to \code{data}.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features only.
#' @param costs [\code{data.frame}]\cr
#'   A numeric matrix or data frame containing the costs of misclassification.
#'   We assume the general case of observation specific costs.
#'   This means we have n rows, corresponding to the observations, in the same order as \code{data}.
#'   The columns correspond to classes and their names are the class labels
#'   (if unnamed we use y1 to yk as labels).
#'   Each entry (i,j) of the matrix specifies the cost of predicting class j
#'   for observation i.
#' @param blocking [\code{factor}]\cr
#'   An optional factor of the same length as the number of observations.
#'   Observations with the same blocking level \dQuote{belong together}.
#'   Specifically, they are either put all in the training or the test set
#'   during a resampling iteration.
#'   Default is \code{NULL} which means no blocking.
#' @param fixup.data [\code{character(1)}]\cr
#'   Should some basic cleaning up of data be performed?
#'   Currently this means removing empty factor levels for the columns.
#'   Possible choices are:
#'   \dQuote{no} = Don't do it.
#'   \dQuote{warn} = Do it but warn about it.
#'   \dQuote{quiet} = Do it but keep silent.
#'   Default is \dQuote{warn}.
#' @param check.data [\code{logical(1)}]\cr
#'   Should sanity of data be checked initially at task creation?
#'   You should have good reasons to turn this off (one might be speed).
#'   Default is \code{TRUE}.
#' @return [\code{\link{Task}}].
#' @examples
#' df = iris
#' cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[df$Species,]
#' df$Species = NULL
#' makeCostSensTask(data = df, cost = cost)
#' @export
#' @family costsens
makeCostSensTask = function(id = deparse(substitute(data)), data, costs, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  # we don't have a target nor weights
  target = character(0L)
  weights = NULL

  if (fixup.data != "no") {
    assert(checkMatrix(costs), checkDataFrame(costs))
    if (is.data.frame(costs))
      costs = as.matrix(costs)
    if (is.null(colnames(costs)))
      colnames(costs) = stri_paste("y", seq_col(costs))
  }
  task = makeSupervisedTask("costsens", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertMatrix(costs, any.missing = FALSE, col.names = "strict")
    assertNumeric(costs, lower = 0)
    if (nrow(costs) != nrow(data))
      stopf("Number of rows in cost matrix (%s) should equal the number of observations (%s).", nrow(costs), nrow(data))
    # we use ..y.. later in the models as a name for temp labels
    if ("..y.." %in% c(colnames(data), colnames(costs)))
      stopf("The name '..y..' is currently reserved for costsens tasks. You can use it neither for features nor labels!")
  }

  task$task.desc = makeCostSensTaskDesc(id, data, target, blocking, costs)
  addClasses(task, "CostSensTask")
}

makeCostSensTaskDesc = function(id, data, target, blocking, costs) {
  td = makeTaskDescInternal("costsens", id, data, target, weights = NULL, blocking = blocking)
  td$class.levels = colnames(costs)
  td$costs = costs
  return(addClasses(td, c("CostSensTaskDesc", "SupervisedTaskDesc")))
}

#' @export
print.CostSensTask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  levs = x$task.desc$class.levels
  catf("Classes: %i\n%s", length(levs), clipString(collapse(levs, sep = ", "), 30L))
}
