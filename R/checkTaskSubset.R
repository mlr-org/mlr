# @template arg_subset
# @param size [int(1)]\cr size of the dataset to subset
# @return numeric vector of subset indicies
checkTaskSubset = function(subset = NULL, size) {

  assert(checkIntegerish(subset, null.ok = TRUE), checkLogical(subset))
  assertInt(size)

  subset = if (is.null(subset)) 
    seq_len(size)
  else if (is.logical(subset))
    which(subset)
  else
    asInteger(subset, min.len = 1L, any.missing = FALSE, lower = 1L, upper = size)

  return(subset)
}
