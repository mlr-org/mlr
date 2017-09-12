#' @title Create a classification task
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
#' @template arg_positive
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @examples
#' if (requireNamespace("mlbench")) {
#'   library(mlbench)
#'   data(BostonHousing)
#'   data(Ionosphere)
#'
#'   makeClassifTask(data = iris, target = "Species")
#'   # an example of a classification task with more than those standard arguments:
#'   blocking = factor(c(rep(1, 51), rep(2, 300)))
#'   makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
#'     positive = "good", blocking = blocking)
#' }
#' @export
makeClassifTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
  # some code on cran passed stuff like positive=1, we can live with the convert here
  if (isScalarNumeric(positive))
    positive = as.character(positive)
  assertString(positive, na.ok = TRUE)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  if (fixup.data != "no") {
    x = data[[target]]
    if (is.character(x) || is.logical(x) || is.integer(x)) {
      data[[target]] = as.factor(x)
    } else if (is.factor(x) && fixup.data == "warn" && hasEmptyLevels(x)) {
      warningf("Target column '%s' contains empty factor levels", target)
      data[[target]] = droplevels(x)
    }
  }
  task = makeSupervisedTask("classif", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
  }

  task$task.desc = makeClassifTaskDesc(id, data, target, weights, blocking, positive)
  addClasses(task, "ClassifTask")
}

makeClassifTaskDesc = function(id, data, target, weights, blocking, positive) {
  levs = levels(data[[target]])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2L)
      positive = levs[1L]
  } else {
    if (m > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
    assertChoice(positive, choices = levs)
  }
  td = makeTaskDescInternal("classif", id, data, target, weights, blocking)
  td$class.levels = levs
  td$positive = positive
  td$negative = NA_character_
  if (length(td$class.levels) == 1L)
    td$negative = stri_paste("not_", positive)
  else if (length(td$class.levels) == 2L)
    td$negative = setdiff(td$class.levels, positive)
  return(addClasses(td, c("ClassifTaskDesc", "SupervisedTaskDesc")))
}

#' @export
print.ClassifTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}
