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

# generates a grid for a vector of features and returns a list
# expand.grid can be applied to this to find all possible combinations of the features
generateFeatureGrid = function(features, data, resample, gridsize, fmin, fmax) {
  sapply(features, function(feature) {
      nunique = length(unique(data[[feature]]))
      cutoff = ifelse(gridsize >= nunique, nunique, gridsize)

      if (is.factor(data[[feature]])) {
        factor(rep(levels(data[[feature]]), length.out = cutoff),
               levels = levels(data[[feature]]), ordered = is.ordered(data[[feature]]))
      } else {
        if (resample != "none") {
          sort(sample(data[[feature]], cutoff, resample == "bootstrap"))
        } else {
          if (is.integer(data[[feature]]))
            sort(rep(fmin[[feature]]:fmax[[feature]], length.out = cutoff))
          else
            seq(fmin[[feature]], fmax[[feature]], length.out = cutoff)
        }
      }
    }, simplify = FALSE)
}

