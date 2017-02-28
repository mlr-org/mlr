#' @title Functional data regression task.
#'
#' @description
#' Functional data regression task.
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
#'   Optional. Named list of grids over which the functional variables are
#'   observed. Each grid of observation points must be provided as a numerical
#'   vector. Default is \code{NULL} then all functional variables are observed
#'   on equidistant observation grids 1, 2, …, <number of observations per
#'   function>.
#' @return [\code{\link{FDARegrTask}}]
#' @export
#' @aliases FDARegrTask
makeFDARegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  task = makeRegrTask(id, data, target, weights, blocking, fixup.data, check.data)
  # arg checks for fd.features adn fd.grids are done in next call
  makeFDATask(task, "fdaregr", fd.features, fd.grids, "FDARegrTask", "FDARegrTaskDesc")
}

#FIXME: to make subsetTask work, otherwise, subsetTask will generate a regression task
makeTaskDesc.FDARegrTask = function(task, id, target, td) {
  badtd = makeTaskDesc.RegrTask(task = task , id = id, target = target)
  badtd$type = "fdaregr"
  badtd$fd.features = td$fd.features
  badtd$fd.grids = td$fd.grids
  addClasses(badtd, "FDARegrTaskDesc")
}
