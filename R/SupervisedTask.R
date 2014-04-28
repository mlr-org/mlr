#' @title Create a classification, regression, survival or cost-sensitive classification task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#'
#' Useful operators are: \code{\link{getTaskFormula}}, \code{\link{getTaskFormulaAsString}},
#' \code{\link{getTaskFeatureNames}}, \code{\link{getTaskData}}, \code{\link{getTaskTargets}},
#' \code{\link{subsetTask}}.
#'
#' Object members:
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored.
#'   Use \code{\link{getTaskData}} in order to access it.}
#' \item{weights [\code{numeric}]}{See argument above. \code{NULL} if not present.}
#' \item{blocking [\code{factor}]}{See argument above. \code{NULL} if not present.}
#' \item{task.desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Id string for object.
#'   Default is the name of R variable passed to \code{data}.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
#' @param target [\code{character(1)} | \code{character(2)}]\cr
#'   Name of the target variable.
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2.
#' @param costs [\code{data.frame}]\cr
#'   A numeric matrix or data frame containing the costs of misclassification.
#'   We assume the general case of observation specific costs.
#'   This means we have n rows, corresponding to the observations, in the same order as \code{data}.
#'   The columns correspond to classes and their names are the class labels
#'   (if unnamed we use y1 to yk as labels).
#'   Each entry (i,j) of the matrix specifies the cost of predicting class j
#'   for observation i.
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
#' @param positive [\code{character(1)}]\cr
#'   Positive class for binary classification.
#'   Default is the first factor level of the target attribute.
#' @param fixup.data [\code{logical(1)}]\cr
#'   Should some basic cleaning up of data be performed?
#'   Currently this means removing empty factor levels for the columns.
#'   Possible coices are:
#'   \dQuote{no} = Don't do it.
#'   \dQuote{warn} = Do it but warn about it.
#'   \dQuote{quiet} = Do it but keep silent.
#'   Default is \dQuote{warn}.
#' @param check.data [\code{logical(1)}]\cr
#'   Should sanity of data be checked initially at task creation?
#'   You should have good reasons to turn this off (one might be speed).
#'   Default is \code{TRUE}
#' @return [\code{\link{SupervisedTask}}].
#' @name SupervisedTask
#' @rdname SupervisedTask
#' @aliases ClassifTask RegrTask SurvTask CostSensTask
#' @examples
#' library(mlbench)
#' data(BostonHousing)
#' data(Ionosphere)
#'
#' makeClassifTask(data = iris, target = "Species")
#' makeRegrTask(data = BostonHousing, target = "medv")
#' # an example of a classification task with more than those standard arguments:
#' blocking = factor(c(rep(1, 51), rep(2, 300)))
#' makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
#'   positive = "good", blocking = blocking)
NULL

makeSupervisedTask = function(subclass, type, data, target, weights = NULL, blocking = NULL,
  check.target.fun, fixup.data = "warn", fixup.fun, check.data = TRUE) {

  checkArg(type, choices = c("classif", "regr", "surv", "costsens"))
  checkArg(data, "data.frame")
  checkArg(check.data, "logical", len = 1L, na.ok = FALSE)
  # check that col names are ok
  if (check.data)
    checkColumnNames(data, 'data')
  # check that target column is ok
  if (check.data)
    check.target.fun(data, target)
  if (!is.null(weights))
    checkArg(weights, "numeric", len = nrow(data), na.ok = FALSE, lower = 0)
  if (!is.null(blocking))
    checkArg(blocking, "factor", len = nrow(data), na.ok = FALSE)
  checkArg(fixup.data, choices = c("warn", "quiet", "no"))
  # check that blocking is ok
  if (check.data)
    checkBlocking(data, target, blocking)
  # possibly convert data a bit
  if (fixup.data != "no")
    data = fixup.fun(data, target, fixup.data)
  # check that data df does not contain bad stuff
  if (check.data)
    checkData(type, data, target)
  # FIXME: don't hash?
  env = new.env(parent = emptyenv())
  env$data = data
  makeS3Obj(c(subclass, "SupervisedTask"),
    env = env,
    weights = weights,
    blocking = blocking
  )
}

# either guess task id from variable name of data or check it
checkOrGuessId = function(id, data) {
  if (missing(id)) {
    # go up to user frame for heuristic to get name of data
    id = deparse(substitute(data, env = parent.frame(1L)))
    if (!is.character(id) || length(id) != 1L)
      stop("Cannot infer id for task automatically. Please set it manually!")
  } else {
    checkArg(id, "character", len = 1L, na.ok = FALSE)
  }
  return(id)
}

#' @S3method print SupervisedTask
print.SupervisedTask = function(x, print.target = TRUE, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Supervised task: %s", td$id)
  catf("Type: %s", td$type)
  if (print.target)
    catf("Target: %s", collapse(td$target))
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse="\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
