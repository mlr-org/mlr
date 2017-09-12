#' @title Create a multilabel task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#'
#' For multilabel classification we assume that the presence of labels is encoded via logical
#' columns in \code{data}. The name of the column specifies the name of the label. \code{target}
#' is then a char vector that points to these columns.
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
#' @param target [\code{character(n.classes)}]\cr
#'   Name(s) of the target variable(s).
#'   For multilabel classification it contains the names of the logical
#'   columns that encode whether a label is present or not and its length corresponds to the
#'   number of classes.
#' @template arg_weights
#' @template arg_blocking
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @examples
#' yeast = getTaskData(yeast.task)
#' labels = colnames(yeast)[1:14]
#' makeMultilabelTask(id = "multi", data = yeast, target = labels)
#' @export
makeMultilabelTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertCharacter(target, any.missing = FALSE, min.len = 2L)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  task = makeSupervisedTask("multilabel", data, target, weights, blocking, fixup.data, check.data)
  # currently we dont do any fixup here
  if (check.data) {
    for (cn in target)
      assertLogical(task$env$data[[cn]], any.missing = FALSE, .var.name = cn)
  }
  task$task.desc = makeMultilabelTaskDesc(id, data, target, weights, blocking)
  addClasses(task, "MultilabelTask")
}

#' @export
print.MultilabelTask = function(x, ...) {
  y = getTaskTargets(x)
  sums = colSums(y)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sums)
}

makeMultilabelTaskDesc = function(id, data, target, weights, blocking) {
  levs = target
  td = makeTaskDescInternal("multilabel", id, data, target, weights, blocking)
  td$class.levels = levs
  return(addClasses(td, c("MultilabelTaskDesc", "SupervisedTaskDesc")))
}
