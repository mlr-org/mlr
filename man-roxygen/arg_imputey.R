#' @param impute.val ([numeric])\cr
#'   If something goes wrong during optimization (e.g. the learner crashes),
#'   this value is fed back to the tuner, so the tuning algorithm does not abort.
#'   It is not stored in the optimization path, an NA and (`character(1)`) corresponding error message are
#'   logged instead.
#'   Note that this value is later multiplied by -1 for maximization measures internally, so you
#'   need to enter `character(1)` larger positive value for maximization here as well.
#'   Default is the worst obtainable value of the performance measure you optimize for when
#'   you aggregate by mean value, or `Inf` instead.
#'   For multi-criteria optimization pass `character(1)` vector of imputation values, one for each of your measures,
#'   in the same order as your measures.
#' @md

