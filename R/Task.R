#' @title Create a classification, regression, survival, cluster, cost-sensitive classification or
#' multilabel task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#' \itemize{
#'  \item{For classification see}{ \code{\link{makeClassifTask}}}
#'  \item{For regression see}{ \code{\link{makeRegrTask}}}
#'  \item{For survival analysis see}{ \code{\link{makeSurvTask}}}
#'  \item{For clustering see}{ \code{\link{makeClusterTask}}}
#'  \item{For cost sensitive classification see}{ \code{\link{makeCostSensTask}}}
#'  \item{For multilabel classification see}{ \code{\link{makeMultilabelTask}}}
#' }
#' @name Task
#' @rdname Task
#' @aliases ClassifTask RegrTask SurvTask CostSensTask ClusterTask MultilabelTask
#' @family task
NULL

#' @inherit Task
#'
#' @param type [\code{character(1)}]\cr The tasktype.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
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
#' @keywords internal
makeTask = function(type, data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE) {
  if (fixup.data != "no") {
    if (fixup.data == "quiet") {
      data = droplevels(data)
    } else if (fixup.data == "warn") {
      # the next lines look a bit complicated, we calculate the warning info message
      dropped = logical(ncol(data))
      for (i in seq_col(data)) {
        x = data[[i]]
        if (is.factor(x) && hasEmptyLevels(x)) {
          dropped[i] = TRUE
          data[[i]] = droplevels(x)
        }
      }
      if (any(dropped))
        warningf("Empty factor levels were dropped for columns: %s", collapse(colnames(data)[dropped]))
    }
  }

  if (check.data) {
    assertDataFrame(data, col.names = "strict")
    if (class(data)[1] != "data.frame") {
      warningf("Provided data is not a pure data.frame but from class %s, hence it will be converted.", class(data)[1])
      data = as.data.frame(data)
    }
    if (!is.null(weights))
      assertNumeric(weights, len = nrow(data), any.missing = FALSE, lower = 0)
    if (!is.null(blocking)) {
      assertFactor(blocking, len = nrow(data), any.missing = FALSE)
      if (length(blocking) && length(blocking) != nrow(data))
        stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
    }
  }

  env = new.env(parent = emptyenv())
  env$data = data
  makeS3Obj("Task",
    type = type,
    env = env,
    weights = weights,
    blocking = blocking,
    task.desc = NA
  )
}

checkTaskData = function(data, cols = names(data)) {
  fun = function(cn, x) {
    if (is.numeric(x)) {
      if (anyInfinite(x))
        stopf("Column '%s' contains infinite values.", cn)
      if (anyNaN(x))
        stopf("Column '%s' contains NaN values.", cn)
    } else if (is.factor(x)) {
      if (hasEmptyLevels(x))
        stopf("Column '%s' contains empty factor levels.", cn)
    } else {
      stopf("Unsupported feature type (%s) in column '%s'.", class(x)[1L], cn)
    }
  }

  Map(fun, cn = cols, x = data[cols])
  invisible(TRUE)
}

#' @export
print.Task = function(x, print.weights = TRUE, ...) {
  td = x$task.desc
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Observations: %i", td$size)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}
