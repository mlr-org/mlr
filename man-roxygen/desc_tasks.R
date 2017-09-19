#' @description
#' The task encapsulates the data and creates a <%= tasktype %> task.
#' It also contains a description object detailing further aspects of the data.
#'
#' <%= randomtext %>
#'
#' Useful operators are <%= operators %>
#' \code{\link{getTaskType}},
#' \code{\link{getTaskFeatureNames}},
#' \code{\link{getTaskNFeats}},
#' \code{\link{getTaskSize}},
#' \code{\link{getTaskData}},
#' \code{\link{getTaskDesc}} and \code{\link{subsetTask}}.
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
