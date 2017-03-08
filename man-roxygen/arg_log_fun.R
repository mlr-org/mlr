#' @param log.fun [\code{function} | \code{character(1)}]\cr
#'   Function used for logging. If set to \dQuote{default} (the default), the evaluated design points, the resulting
#'   performances, and the runtime will be reported.
#'   If set to \dQuote{memory}, the memory usage for each evaluation will also be displayed, with a small increase
#'   in run time.
#'   Otherwise a function with arguments \code{learner}, \code{resampling}, \code{measures},
#'   \code{par.set}, \code{control}, \code{opt.path}, \code{dob}, \code{x}, \code{y}, \code{remove.nas},
#'   \code{stage}, and \code{prev.stage} is expected.
#'   The default displays the performance measures, the time needed for evaluating,
#'   the currently used memory and the max memory ever used before
#'   (the latter two both taken from \code{\link{gc}}).
#'   See the implementation for details.

