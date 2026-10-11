#' @param same.resampling.instance (`logical(1)`)\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is `TRUE`.
#' @param start ([list])\cr
#'   Named list of initial parameter values.
#' @param tune.threshold (`logical(1)`)\cr
#'   Should the threshold be tuned for the measure at hand, after each hyperparameter evaluation,
#'   via [tuneThreshold]?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is `FALSE`.
#' @param tune.threshold.args ([list])\cr
#'   Further arguments for threshold tuning that are passed down to [tuneThreshold].
#'   Default is none.
#' @param final.dw.perc (`boolean`)\cr
#'   If a Learner wrapped by a [makeDownsampleWrapper] is used, you can define the value of `dw.perc` which is used to train the Learner with the final parameter setting found by the tuning.
#'   Default is `NULL` which will not change anything.
#' @param ... (any)\cr
#'   Further control parameters passed to the `control` arguments of
#'   [cmaes::cma_es] or [GenSA::GenSA], as well as
#'   towards the `tunerConfig` argument of [irace::irace].
#' @param budget (`integer(1)`)\cr
#'   Maximum number of function evaluations. See the individual tuning controls
#'   for their defaults.
