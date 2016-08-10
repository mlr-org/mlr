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

# matches factor levels of data.frames in a list
matchFactorLevels = function(lst) {
  if (length(lst) > 1L) {
    targets = unlist(lapply(lst, function(df) lapply(df, class)), FALSE)
    targets = targets[which(targets == "factor")]
    targets = unique(names(targets))
    if (length(unique(sapply(targets, length))) != 1L)
      stop("There must be an equal number of factors in each data.frame.")

    levs = unlist(lapply(lst, function(df) lapply(df[targets], levels)), FALSE)
    ret = lapply(unique(names(levs)), function(x)
      unique(unname(do.call("c", levs[names(levs) == x]))))
    names(ret) = unique(names(levs))

    lapply(lst, function(df) {
      for (x in targets)
        df[, x] = factor(df[, x], levels = ret[[x]])
      df
    })
  } else {
    lst
  }
}
