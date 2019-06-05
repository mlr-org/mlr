#' @param keep.pred (`logical(1)`)\cr
#'   Keep the prediction data in the `pred` slot of the result object.
#'   If you do many experiments (on larger data sets) these objects might unnecessarily increase
#'   object size / mem usage, if you do not really need them.
#'   This is why the default is set to `FALSE`.
#' @param keep.extract (`logical(1)`)\cr
#'   Keep the `extract` slot of the result object. When creating a lot of
#'   benchmark results with extensive tuning, the resulting R objects can become
#'   very large in size. That is why the tuning results stored in the `extract`
#'   slot are removed by default (`keep.extract = FALSE`). Note that when
#'   `keep.extract = FALSE` you will not be able to conduct analysis in the
#'   tuning results.
#' @md
