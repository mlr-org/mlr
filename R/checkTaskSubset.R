# @template arg_subset
# @param size [int(1)]\cr size of the dataset to subset
# @return numeric vector of subset indicies
checkTaskSubset = function(subset = NULL, size) {
  assertCount(size)
  if (is.null(subset)) {
    subset = seq_len(size)
  } else {
    if (is.logical(subset))
      subset = which(subset)
    subset = asInteger(subset, min.len = 1L, any.missing = FALSE, lower = 1L, upper = size)
  }
  subset
}
