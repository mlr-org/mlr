#' @param drop [\code{logical(1)}]\cr
#'   If drop is \code{FALSE} (the default), a nested list with
#'   the following structure is returned:\cr
#'   \code{res[task.ids][learner.ids]}.\cr
#'   If drop is set to \code{TRUE}, it is checked if the list
#'   structure can be simplified.\cr
#'   If only one learner was passed, a list with entries
#'   for each task is returned.\cr
#'   If only one task was passed, the entries are named after
#'   the corresponding learner.\cr
#'   For an experiment with both one task and learner,
#'   the whole list structure is removed.\cr
#'   Note that the name of the
#'   task/learner will be dropped from the return object.

