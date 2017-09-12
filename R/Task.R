#' @title Create a classification, regression, survival, cluster, cost-sensitive classification or
#' multilabel task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#'
#' For classification see \code{\link{makeClassifTask}}.
#'
#' For regression see \code{\link{makeRegrTask}}.
#'
#' For survival analysis see \code{\link{makeSurvTask}}.
#'
#' For clustering see \code{\link{makeClusterTask}}.
#'
#' For cost sensitive classification see \code{\link{makeCostSensTask}}.
#'
#' For multilabel see \code{\link{makeMultilabelTask}}.
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
#' Notes:
#' For multilabel classification we assume that the presence of labels is encoded via logical
#' columns in \code{data}. The name of the column specifies the name of the label. \code{target}
#' is then a char vector that points to these columns.
#' Functional data can be added to a task via matrix columns. For more information refer to
#' \code{\link{makeFunctionalData}}.
#'
#' @template arg_id
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
#' @param target [\code{character(1)} | \code{character(2)} | \code{character(n.classes)}]\cr
#'   Name(s) of the target variable(s).
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2. For multilabel classification it contains the names of the logical
#'   columns that encode whether a label is present or not and its length corresponds to the
#'   number of classes.
#' @template arg_costs
#' @template arg_weights
#' @template arg_blocking
#' @template arg_positive
#' @template arg_fixup.data
#' @template arg_check.data
#' @return [\code{\link{Task}}].
#' @name Task
#' @rdname Task
#' @aliases ClassifTask RegrTask SurvTask CostSensTask ClusterTask MultilabelTask
#' @examples
#' if (requireNamespace("mlbench")) {
#'   library(mlbench)
#'   data(BostonHousing)
#'   data(Ionosphere)
#'
#'   makeClassifTask(data = iris, target = "Species")
#'   makeRegrTask(data = BostonHousing, target = "medv")
#'   # an example of a classification task with more than those standard arguments:
#'   blocking = factor(c(rep(1, 51), rep(2, 300)))
#'   makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
#'     positive = "good", blocking = blocking)
#'   makeClusterTask(data = iris[, -5L])
#' }
NULL

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
