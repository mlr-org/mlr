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
}
