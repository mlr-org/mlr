#' @param keep.extract (`logical(1)`)\cr
#'   Keep the `extract` slot of the result object. When creating a lot of
#'   benchmark results with extensive tuning, the resulting R objects can become
#'   very large in size. That is why the tuning results stored in the `extract`
#'   slot are removed by default (`keep.extract = FALSE`). Note that when
#'   `keep.extract = FALSE` you will not be able to conduct analysis in the
#'   tuning results.
#' @md
