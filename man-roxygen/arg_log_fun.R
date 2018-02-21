#' @param log.fun (`function` | `character(1)`)\cr
#'   Function used for logging. If set to \dQuote{default} (the default), the evaluated design points, the resulting
#'   performances, and the runtime will be reported.
#'   If set to \dQuote{memory} the memory usage for each evaluation will also be displayed, with `character(1)` small increase
#'   in run time.
#'   Otherwise `character(1)` function with arguments `learner`, `resampling`, `measures`,
#'   `par.set`, `control`, `opt.path`, `dob`, `x`, `y`, `remove.nas`,
#'   `stage` and `prev.stage` is expected.
#'   The default displays the performance measures, the time needed for evaluating,
#'   the currently used memory and the max memory ever used before
#'   (the latter two both taken from [gc]).
#'   See the implementation for details.
#' @md
