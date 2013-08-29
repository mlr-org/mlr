#' Generate a fixed holdout instance for resampling.
#'
#' @param train.inds [\code{integer}]\cr
#'   Indices for training set.
#' @param test.inds [\code{integer}]\cr
#'   Indices for test set.
#' @param size [\code{integer(1)}]\cr
#'   Size of the data set to resample.
#' @return [\code{\link{ResampleInstance}}].
#' @export
makeFixedHoldoutInstance = function(train.inds, test.inds, size) {
  train.inds = convertIntegers(train.inds)
  test.inds = convertIntegers(test.inds)
  size = convertInteger(size)
  # FIXME min.len
  checkArg(train.inds, "integer", na.ok=FALSE)
  checkArg(test.inds, "integer", na.ok=FALSE)
  checkArg(size, "integer", len=1L, na.ok=FALSE)
  # FIXME DIV/0
  rdesc = makeResampleDesc("Holdout", split=length(train.inds)/size)
  rin = makeResampleInstance(rdesc, size=size)
  rin$train.inds[[1L]] = train.inds
  rin$test.inds[[1L]] = test.inds
  return(rin)
}
