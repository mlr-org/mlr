#' @title Functional data classification task.
#'
#' @description
#' Create a functional data classification task. This means that some features
#' in the task will be so-called functional covariates / functional featutes,
#' measured on a grid or time scale.
#'
#' @inheritParams Task
#' @param fd.features [\code{list}]\cr
#'   Optional. Named list of column numbers or column names. Each list entry
#'   defines one functional variable through either a character vector of column
#'   names or an integer vector of column indices. The list names specify the
#'   names of the functional variables. All columns that are not referenced in
#'   \code{fd.features} are scalar variables. Default is \code{“fd1”} which
#'   means we assume that all columns form one functional variable.
#' @param fd.grids [\code{list}]\cr
#'   Optional. Named list of grids over which the functional variables are observed.
#'   Each grid of observation points must be provided as a numerical vector.
#'   Default is \code{NULL} then all functional variables are observed on equidistant
#'   observation grids 1, 2, …, <number of observations per function>.
#' @return [\code{\link{FDAClassifTask}}]
#' @export
#' @aliases FDAClassifTask
makeFDAClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn",
  check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  # arg checks for fd.features adn fd.grids are done in next call
  makeFDATask(task, "fdaclassif", fd.features, fd.grids, "FDAClassifTask", "FDAClassifTaskDesc")
}
