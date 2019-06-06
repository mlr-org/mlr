# @template arg_subset
# @param size [int(1)]\cr size of the dataset to subset
# @return numeric vector of subset indices
checkTaskSubset = function(subset = NULL, size) {
  assertCount(size)
  if (is.null(subset)) {
    seq_len(size)
  } else if (is.logical(subset)) {
    subset = which(subset)
    assertInteger(subset, min.len = 1L, upper = size)
  } else {
    asInteger(subset, min.len = 1L, any.missing = FALSE, lower = 1L, upper = size)
  }
}
