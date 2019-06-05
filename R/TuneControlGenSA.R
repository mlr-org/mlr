#' @title Create control object for hyperparameter tuning with GenSA.
#'
#' @description
#' Generalized simulated annealing with method [GenSA::GenSA].
#' Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#' For integers the internally proposed numeric values are automatically rounded.
#'
#' @inherit TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. [GenSA::GenSA] defines the `budget` via
#'   the argument `max.call`. However, one should note that this algorithm
#'   does not stop its local search before its end. This behavior might lead
#'   to an extension of the defined budget and will result in a warning.
#' @aliases TuneControlGenSA
#' @family tune
#' @return ([TuneControlGenSA]).
#' @export
makeTuneControlGenSA = function(same.resampling.instance = TRUE, impute.val = NULL,
  start = NULL, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = "default",
  final.dw.perc = NULL, budget = NULL, ...) {
  args = list(...)
  if (is.null(budget)) {
    if (!is.null(args$max.call)) {
      budget = args$max.call
    } else {
      budget = args$max.call = 1e+07
    }
  } else {
    if (is.null(args$max.call)) {
      args$max.call = budget
    } else {
      if (args$max.call != budget) {
        stopf("The given budget (%i) contradicts to the maximum number of function evaluations (max.call = %i).",
          budget, args$max.call)
      }
    }
  }
  args$max.call = asCount(args$max.call)

  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, cl = "TuneControlGenSA")
  do.call(makeTuneControl, c(args, args2))
}
