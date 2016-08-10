# get one el from each row of a matrix, given indices or col names
getRowEls = function(mat, inds) {
  if (is.character(inds))
    inds = match(inds, colnames(mat))
  inds = cbind(seq_row(mat), inds)
  mat[inds]
}

# get one el from each col of a matrix, given indices or row names
getColEls = function(mat, inds) {
  getRowEls(t(mat), inds)
}

# prints more meaningful 'head' output indicating that there is more output
printHead = function(x, n = 6L, ...) {
  print(head(x, n = n, ...))
  if (nrow(x) > n)
    catf("... (%i rows, %i cols)\n", nrow(x), ncol(x))

# Do fuzzy string matching between input and a set of valid inputs
# and return the most similar valid inputs.
getNameProposals = function(input, possible.inputs, nproposals = 3L) {
  assertString(input)
  assertCharacter(possible.inputs)
  assertInt(nproposals, lower = 1L)

  # compute the approximate string distance (using the generalized Levenshtein / edit distance)
  # and get the nproposals most similar valid inputs.
  indices = order(adist(input, possible.inputs))[1:nproposals]
  possibles = na.omit(possible.inputs[indices])
  return(possibles)
}
