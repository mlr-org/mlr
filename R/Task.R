#' @title Create a classification, regression, survival, cluster, cost-sensitive classification or
#' multilabel task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#' It also contains a description object detailing further aspects of the data.
#'
#' Useful operators are:
#' - [getTaskFormula],
#' - [getTaskFeatureNames],
#' - [getTaskData],
#' - [getTaskTargets], and
#' - [subsetTask].
#'
#' Object members:
#' \describe{
#' \item{env (`environment`)}{Environment where data for the task are stored.
#'   Use [getTaskData] in order to access it.}
#' \item{weights ([numeric])}{See argument. `NULL` if not present.}
#' \item{blocking ([factor])}{See argument. `NULL` if not present.}
#' \item{task.desc ([TaskDesc])}{Encapsulates further information about the task.}
#' }
#'
#' Functional data can be added to a task via matrix columns. For more information refer to
#' [makeFunctionalData].
#'
#' @param id (`character(1)`)\cr
#'   Id string for object.
#'   Default is the name of the R variable passed to `data`.
#' @param data ([data.frame])\cr
#'   A data frame containing the features and target variable(s).
#' @param target (`character(1)` | `character(2)` | `character(n.classes)`)\cr
#'   Name(s) of the target variable(s).
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2. For multilabel classification it contains the names of the logical
#'   columns that encode whether a label is present or not and its length corresponds to the
#'   number of classes.
#' @param costs ([data.frame])\cr
#'   A numeric matrix or data frame containing the costs of misclassification.
#'   We assume the general case of observation specific costs.
#'   This means we have n rows, corresponding to the observations, in the same order as `data`.
#'   The columns correspond to classes and their names are the class labels
#'   (if unnamed we use y1 to yk as labels).
#'   Each entry (i,j) of the matrix specifies the cost of predicting class j
#'   for observation i.
#' @param weights ([numeric])\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   Cannot be set for cost-sensitive learning.
#'   Default is `NULL` which means no (= equal) weights.
#' @param blocking ([factor])\cr
#'   An optional factor of the same length as the number of observations.
#'   Observations with the same blocking level \dQuote{belong together}.
#'   Specifically, they are either put all in the training or the test set
#'   during a resampling iteration.
#'   Default is `NULL` which means no blocking.
#' @param positive (`character(1)`)\cr
#'   Positive class for binary classification (otherwise ignored and set to NA).
#'   Default is the first factor level of the target attribute.
#' @param fixup.data (`character(1)`)\cr
#'   Should some basic cleaning up of data be performed?
#'   Currently this means removing empty factor levels for the columns.
#'   Possible choices are:
#'   \dQuote{no} = Don't do it.
#'   \dQuote{warn} = Do it but warn about it.
#'   \dQuote{quiet} = Do it but keep silent.
#'   Default is \dQuote{warn}.
#' @param check.data (`logical(1)`)\cr
#'   Should sanity of data be checked initially at task creation?
#'   You should have good reasons to turn this off (one might be speed).
#'   Default is `TRUE`.
#' @param coordinates ([data.frame])\cr
#'   Coordinates of a spatial data set that will be used for spatial partitioning of the data in a spatial cross-validation resampling setting.
#'   Coordinates have to be numeric values.
#'   Provided [data.frame] needs to have the same number of rows as data and consist of at least two dimensions.
#' @return [Task].
#' @name Task
#' @seealso [ClassifTask] [ClusterTask] [CostSensTask] [MultilabelTask] [RegrTask] [SurvTask]
#' @rdname Task
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

#' Exported for internal use.
#' @param id (`character(1)`)\cr
#'   task id
#' @param data ([data.frame])\cr
#'   data
#' @param target ([character])\cr
#'   target columns
#' @param weights ([numeric])\cr
#'   weights
#' @param blocking ([numeric`\cr
#'   task data blocking
#' @param coordinates ([data.frame])\cr
#'   Coordinates of a spatial data set that will be used for spatial partitioning of the data in a spatial cross-validation resampling setting.
#'   Coordinates have to be numeric values.
#'   Provided ([data.frame]) needs to have the same number of rows as data and consist of at least two dimensions.
#' @keywords internal
#' @name makeTaskDesc
NULL

makeTask = function(type, data, weights = NULL, blocking = NULL, fixup.data = "warn", check.data = TRUE, coordinates = NULL) {
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
      if (any(dropped)) {
        warningf("Empty factor levels were dropped for columns: %s", collapse(colnames(data)[dropped]))
      }
    }
  }

  if (check.data) {
    assertDataFrame(data, col.names = "strict")
    if (class(data)[1] != "data.frame") {
      warningf("Provided data is not a pure data.frame but from class %s, hence it will be converted.", class(data)[1])
      data = as.data.frame(data)
    }
    if (!is.null(weights)) {
      assertNumeric(weights, len = nrow(data), any.missing = FALSE, lower = 0)
    }
    if (!is.null(blocking)) {
      assertFactor(blocking, len = nrow(data), any.missing = FALSE)
      if (length(blocking) && length(blocking) != nrow(data)) {
        stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
      }
    }
    if (!is.null(coordinates)) {
      if (nrow(coordinates) != nrow(data)) {
        stop("Coordinates need to have the same length data! Or pass none at all.")
      }
      if (ncol(coordinates) < 2) {
        stop("Supplied coordinates need to consist of at least two dimensions.")
      }
      if (!is.data.frame(coordinates)) {
        warningf("Provided coordinates are not given as a data frame but as class %s. Please provide a data frame.", class(coordinates))
      }
    }
  }

  env = new.env(parent = emptyenv())
  env$data = data
  makeS3Obj("Task",
    type = type,
    env = env,
    weights = weights,
    blocking = blocking,
    coordinates = coordinates,
    task.desc = NA
  )
}

checkTaskData = function(data, cols = names(data)) {
  fun = function(cn, x) {
    if (is.numeric(x)) {
      if (anyInfinite(x)) {
        stopf("Column '%s' contains infinite values.", cn)
      }
      if (anyNaN(x)) {
        stopf("Column '%s' contains NaN values.", cn)
      }
    } else if (is.factor(x)) {
      if (hasEmptyLevels(x)) {
        stopf("Column '%s' contains empty factor levels.", cn)
      }
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
  if (print.weights) {
    catf("Has weights: %s", td$has.weights)
  }
  catf("Has blocking: %s", td$has.blocking)
  catf("Has coordinates: %s", td$has.coordinates)
}
