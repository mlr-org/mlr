#' @title Create a classification, regression, survival, cluster, cost-sensitive classification,
#' multilabel task or one-class classification.
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
#' Note on multilabel:
#' For multilabel classification we assume that the presence of labels is encoded via logical
#' columns in \code{data}. The name of the column specifies the name of the label. \code{target}
#' is then a char vector that points to these columns.
#'
#' Note on one-class classification:
#' One-class classification problem is an unsupervised learning problem, but we still require
#' to define a target column, in order to allow supervised evalaution if labels are available.
#' This class columns should be a factor, where the levels are the strings denoted by
#' \code{positive} and \code{negative}, where the former denotes the name of the anomaly class
#' and the latter the name of the normal class.
#'
#' Functional data can be added to a task via matrix columns. For more information refer to
#' \code{\link{makeFunctionalData}}.
#'
#' @param id [\code{character(1)}]\cr
#'   Id string for object.
#'   Default is the name of the R variable passed to \code{data}.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
#' @param target [\code{character(1)} | \code{character(2)} | \code{character(n.classes)}]\cr
#'   Name(s) of the target variable(s).
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2. For multilabel classification it contains the names of the logical
#'   columns that encode whether a label is present or not and its length corresponds to the
#'   number of classes.
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
#'   Positive class for binary and one-class classification (otherwise ignored and set to NA).
#'   For one-class classification this is the name of the \dQuote{anomaly} class and there is no default.
#'   Default is the first factor level of the target attribute).
#' @param negative [\code{character(1)}]\cr
#'   Negative class name, currently only used in one-class classification, here it encodes the name
#'   of the \dQuote{normal} class.
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
#' @name Task
#' @rdname Task
#' @aliases ClassifTask RegrTask SurvTask CostSensTask ClusterTask MultilabelTask OneClassTask
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
#'}
#'
#' # for anomaly create example data with 5% anomalies
#' set.seed(123)
#' sigma = matrix(c(2, 0, 0, 5, 0, 0), 2, 2)
#' normal = as.data.frame(mvrnorm(n = 1000, rep(0, 2), sigma))
#' normal$Target = "Normal"
#'
#' anomaly = as.data.frame(matrix(sample(size = 50 * 2, x = 20:100, replace = TRUE), 50, 2))
#' anomaly$Target = "Anomaly"
#' data = rbind(normal, anomaly)
#' data = na.omit(data)
#'
#' # create tasks, it is required to set the positive class (anomaly class) and
#' the negative class (normal class) as well as the name of the target column
#' oneclass2d.task = makeOneClassTask("one-class-2d-example", data = data,
#' target = "Target", positive = "Anomaly", negative = "Normal")
#'
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
