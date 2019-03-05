#' @param keep.pred (`logical(1)`)\cr
#'   Keep the prediction data in the `pred` slot of the result object.
#'   If you do many experiments (on larger data sets) these objects might unnecessarily increase
#'   object size / mem usage, if you do not really need them.
#'   In this case you can set this argument to `FALSE`.
#'   Default is `TRUE`.
#' @param keep.extract (`logical(1)`)\cr
#'   Keep the `extract` slot of the result object.
#'   When creating a lot of benchmark results with extensive tuning, the
#'   resulting objects can become vary large in size.
#'   Note that when you remove this slot, you will not be able to make
#'   post analysis on the tuning effects.
#' @md
