#' Create a classification, regression or survival task for a given data set.
#'
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task (either classification, regression or survival),
#' and contains a description object detailing further aspects of the data.
#'
#' Useful operators are: \code{\link{getTaskFormula}}, \code{\link{getTaskFormulaAsString}}, \code{\link{getTaskFeatureNames}},
#' \code{\link{getTaskData}}, \code{\link{getTaskTargets}}, \code{\link{subsetTask}}.
#'
#' Object members:
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored. Use \code{\link{getTaskData}} in order to access it.}
#' \item{blocking [\code{factor}]}{See argument above. \code{factor(0)} if not present.}
#' \item{task.desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
#' }
#' @param id [\code{character(1)}]\cr
#'   Id string for object.
#'   Default is the name of R variable passed to \code{data}.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable.
#' @param target [\code{character(1)}]\cr
#'   Name of the target variable.
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#' @param blocking [\code{factor}]\cr
#'   An optional factor of the same length as the number of observations.
#'   Observations with the same blocking level \dQuote{belong together}.
#'   Specifically, they are either put all in the training or the test set
#'   during a resampling iteration.
#'   Default is no blocking.
#' @param positive [\code{character(1)}]\cr
#'   Positive class for binary classification.
#'   Default is the first factor level of the target attribute.
#' @param fixup.data [\code{logical(1)}]\cr
#'   Should some basic cleaning up of data be performed?
#'   Currently this means removing empty factor levels for the columns.
#'   Possible coices are: \dQuote{no} = Don't do it. \dQuote{warn} = Do it but warn about it.
#'   \dQuote{quiet} = Do it but keep silent.
#'   Default is \dQuote{warn}.
#' @param check.data [\code{logical(1)}]\cr
#'   Should sanity of data be checked initially at task creation?
#'   You should have good reasons to turn this off.
#'   Default is \code{TRUE}
#' @return [\code{\link{SupervisedTask}}].
#' @name SupervisedTask
#' @rdname SupervisedTask
#' @aliases ClassifTask RegrTask SurvTask
#' @examples
#' library(mlbench)
#' data(BostonHousing)
#' RegrTask <- makeRegrTask(data = BostonHousing, target = "medv")
#'
#' ClassifTask <- makeClassifTask(data = iris, target = "Species")
#'
#' ## an example of a classification task with more than those standard arguments:
#' library(mlbench)
#' data(Ionosphere)
#' blocks <- factor(c(rep(1, 51), rep(2, 300)))
#' ClassifTask <- makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
#'                                positive = "good", blocking = blocks)
NULL

makeSupervisedTask = function(type, id, data, target, weights, blocking, positive, fixup.data, check.data) {
  if(missing(id)) {
    id = deparse(substitute(data))
    if (!is.character(id) || length(id) != 1L)
      stop("Cannot infer id for task automatically. Please set it manually!")
  } else {
    checkArg(id, "character", len=1L, na.ok=FALSE)
  }
  checkArg(data, "data.frame")
  checkArg(target, "character", min.len=1L, na.ok=FALSE)
  if (!missing(weights)) {
    checkArg(weights, "numeric", len=nrow(data), na.ok=FALSE, lower=0)
  } else {
    weights = NULL
  }
  if (missing(blocking))
    blocking = factor()
  else
    checkArg(blocking, "factor", len=nrow(data), na.ok=FALSE)
  checkArg(fixup.data, choices = c("warn", "quiet", "no"))
  checkArg(check.data, "logical", len=1L, na.ok=FALSE)
  checkBlocking(data, target, blocking)
  checkColumnNames(data, target)
  data = fixupData(data, fixup.data)
  if (type == "classif") {
    if (length(target) != 1L)
      stop("Exactly one target column must be specified for a classification task")

    if (!is.factor(data[, target])) {
      if (is.character(data[, target]) || is.logical(data[, target]))
        data[, target] = as.factor(data[, target])
      else
        stopf("Target column %s has an unsupported type for classification. Either you made a mistake or you have to convert it. Type: %s",
          target, class(data[,target])[1L])
    }
    levs = levels(data[, target])
    m = length(levs)
    if (missing(positive)) {
      if (m <= 2L)
        positive = levs[1L]
      else
        positive = NA_character_
    } else {
      if (m > 2L)
        stop("Cannot set a positive class for a multiclass problem!")
      checkArg(positive, choices=levs)
    }
  } else if (type == "regr") {
    if (length(target) != 1L)
      stop("Exactly one target column must be specified for a regression task")
    if (is.integer(data[, target]))
      data[, target] = as.numeric(data[, target])
    else if(!is.numeric(data[, target]))
      stopf("Target column %s has an unsupported type for regression. Either you made a mistake or you have to convert it. Type: %s",
        target, class(data[,target])[1L])
    positive = NA_character_
  } else if (type == "surv") {
    if (length(target) != 2L)
      stop("Exactly two target columns must be specified for a survival task")
    # target = setNames(target, c("time", "status"))
    if (!is.numeric(data[[target[1L]]]))
      stop("Survival time must be numeric")
    if (!is.logical(data[[target[2L]]]) || is.integer(data[[target[2L]]]))
      data[[target[2L]]] = as.integer(as.logical(data[[target[2L]]]))
    positive = NA_character_
  }
  if (check.data)
    checkData(data, target)
  desc = makeTaskDesc(type, id, data, target, weights, blocking, positive)
  # FIXME: don't hash?
  env = new.env(parent=emptyenv())
  env$data = data
  setClasses(list(
    env = env,
    task.desc = desc,
    weights = weights,
    blocking = blocking
  ), "SupervisedTask")
}

#' @S3method print SupervisedTask
print.SupervisedTask = function(x, ...) {
  td = x$task.desc
  catf("Supervised task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Target: %s", collapse(td$target))
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse="\n"))
  catf("Missings: %s", td$has.missings)
  catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
