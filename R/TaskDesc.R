#' Description object for task.
#'
#' Description object for task, encapsulates basic properties of the task
#' without having to store the complete data set.
#'
#' Object members:
#' \describe{
#' \item{id (`character(1)`)}{Id string of task.}
#' \item{type (`character(1)`)}{Type of task, \dQuote{classif} for classification,
#'   \dQuote{regr} for regression, \dQuote{surv} for survival and \dQuote{cluster} for
#'   cluster analysis, \dQuote{costsens} for cost-sensitive classification, and
#'   \dQuote{multilabel} for multilabel classification.}
#' \item{target (`character(0)` | `character(1)` | `character(2)` | `character(n.classes)`)}{
#'   Name(s) of the target variable(s).
#'   For \dQuote{surv} these are the names of the survival time and event columns, so it has length 2.
#'   For \dQuote{costsens} it has length 0, as there is no target column, but a cost matrix instead.
#'   For \dQuote{multilabel} these are the names of logical columns that indicate whether a
#'   class label is present and the number of target variables corresponds to the number of
#'   classes.}
#' \item{size (`integer(1)`)}{Number of cases in data set.}
#' \item{n.feat (`integer(2)`)}{Number of features, named vector with entries:
#'   \dQuote{numerics}, \dQuote{factors}, \dQuote{ordered}, \dQuote{functionals}.}
#' \item{has.missings (`logical(1)`)}{Are missing values present?}
#' \item{has.weights (`logical(1)`)}{Are weights specified for each observation?}
#' \item{has.blocking (`logical(1)`)}{Is a blocking factor for cases available in the task?}
#' \item{class.levels ([character])}{All possible classes.
#'   Only present for \dQuote{classif}, \dQuote{costsens}, and \dQuote{multilabel}.}
#' \item{positive (`character(1)`)}{Positive class label for binary classification.
#'   Only present for \dQuote{classif}, NA for multiclass.}
#' \item{negative (`character(1)`)}{Negative class label for binary classification.
#'   Only present for \dQuote{classif}, NA for multiclass.}
#' }
#' @name TaskDesc
#' @rdname TaskDesc
NULL

#' Exported for internal use.
#' @param type (`character(1)`)\cr
#'   Task type.
#' @param id (`character(1)`)\cr
#'   task id
#' @param data ([data.frame])\cr
#'   data
#' @param target ([character])\cr
#'   target columns
#' @param weights ([numeric])\cr
#'   weights
#' @param blocking ([numeric])\cr
#'   task data blocking
#' @param coordinates (`logical(1)`)\cr
#'   whether spatial coordinates have been provided
#' @keywords internal
#' @export
makeTaskDescInternal = function(type, id, data, target, weights, blocking, coordinates) {
  # get classes of feature cols
  cl = vcapply(data, function(x) class(x)[1L])
  cl = table(dropNamed(cl, target))
  n.feat = c(
    numerics = sum(cl[c("integer", "numeric")], na.rm = TRUE),
    factors = sum(cl["factor"], na.rm = TRUE),
    ordered = sum(cl["ordered"], na.rm = TRUE),
    functionals = sum(cl["matrix"], na.rm = TRUE)
  )

  makeS3Obj("TaskDesc",
    id = id,
    type = type,
    target = target,
    size = nrow(data),
    n.feat = n.feat,
    has.missings = anyMissing(data),
    has.weights = !is.null(weights),
    has.blocking = !is.null(blocking),
    has.coordinates = !is.null(coordinates)
  )
}
