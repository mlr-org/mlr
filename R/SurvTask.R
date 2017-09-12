#' @title Create a survival task.
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
#'   A data frame containing the features and target variable(s).
#' @param target [\code{character(2)}]\cr
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2.
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   Cannot be set for cost-sensitive learning.
#'   Default is \code{NULL} which means no (= equal) weights.
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
#' @param censoring [\code{character(1)}]\cr
#'  Censoring type. Allowed choices are \dQuote{rcens} for right censored data (default),
#'  \dQuote{lcens} for left censored and \dQuote{icens} for interval censored data using
#'  the \dQuote{interval2} format.
#'  See \code{\link[survival]{Surv}} for details.
#' @return [\code{\link{Task}}].
#' @examples
#' data(lung, package = "survival")
#' lung$status = (lung$status == 2) # convert to logical
#' makeSurvTask(data = lung, target = c("time", "status"))
#' @export
makeSurvTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertCharacter(target, any.missing = FALSE, len = 2L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)


  if (fixup.data != "no") {
    time = data[[target[1L]]]
    event = data[[target[2L]]]

    if (is.integer(time))
      data[[target[1L]]] = as.double(time)

    if (is.numeric(event)) {
      if (testIntegerish(event) && all(as.integer(event) %in% c(0L, 1L)))
        data[[target[2L]]] = (as.integer(event) == 1L)
    } else if (is.factor(event)) {
      lvls = levels(event)
      if (length(lvls) == 2L) {
        if (all(lvls %in% c("TRUE", "FALSE"))) {
          data[[target[2L]]] = (event == "TRUE")
        } else if (all(lvls %in% c("0", "1"))) {
          data[[target[2L]]] = (as.character(event) == "1")
        }
      }
    }
  }

  task = makeSupervisedTask("regr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    time = data[[target[1L]]]
    event = data[[target[2L]]]
    assertNumeric(time, lower = 0, finite = TRUE, any.missing = FALSE, .var.name = "target column time")
    assertLogical(event, any.missing = FALSE, .var.name = "target column event")
  }

  task$task.desc = makeSurvTaskDesc(id, data, target, weights, blocking)
  addClasses(task, "SurvTask")
}

makeSurvTaskDesc = function(id, data, target, weights, blocking) {
  td = makeTaskDescInternal("surv", id, data, target, weights, blocking)
  addClasses(td, c("SurvTaskDesc", "SupervisedTaskDesc"))
}
