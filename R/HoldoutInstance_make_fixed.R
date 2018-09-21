#' Generate a fixed holdout instance for resampling.
#'
#' @param train.inds ([integer])\cr
#'   Indices for training set.
#' @param test.inds ([integer])\cr
#'   Indices for test set.
#' @param size (`integer(1)`)\cr
#'   Size of the data set to resample.
#'   The function needs to know the largest possible index of the whole data set.
#' @return ([ResampleInstance]).
#' @export
makeFixedHoldoutInstance = function(train.inds, test.inds, size) {
  train.inds = asInteger(train.inds, any.missing = FALSE)
  test.inds = asInteger(test.inds, any.missing = FALSE)
  size = asInt(size, lower = 1L)
  rdesc = makeResampleDesc("Holdout", split = length(train.inds) / size)
  rin = makeResampleInstance(rdesc, size = size)
  rin$train.inds[[1L]] = train.inds
  rin$test.inds[[1L]] = test.inds
  return(rin)
}
