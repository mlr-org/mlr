# get one el from each row of a matrix, given indices or col names (factors for colnames are converted to characters)
getRowEls = function(mat, inds) {
  if (is.factor(inds))
    inds = as.character(inds)
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

# generates a grid for a vector of features and returns a list
# expand.grid can be applied to this to find all possible combinations of the features
generateFeatureGrid = function(features, data, resample, gridsize, fmin, fmax) {
  sapply(features, function(feature) {
      nunique = length(unique(data[[feature]]))
      cutoff = ifelse(gridsize >= nunique, nunique, gridsize)
      if (resample == "none") {
        switch(paste0(class(data[[feature]]), collapse = ":"),
          "integer" = as.integer(seq.int(fmin[[feature]], fmax[[feature]], length.out = cutoff)),
          "numeric" = seq(fmin[[feature]], fmax[[feature]], length.out = cutoff),
          "ordered:factor" = sort(unique(data[[feature]]))[as.integer(seq.int(1, nunique, length.out = cutoff))],
          "factor" = sample(unique(data[[feature]]), size = cutoff) ## impossible to order selection if cutoff < nunique w/o ordering
        )

      } else {
        if (is.ordered(data[[feature]])) {
          sort(sample(data[[feature]], size = cutoff, replace = resample == "bootstrap"))
        } else {
          sample(data[[feature]], size = cutoff, replace = resample == "bootstrap")
        }
      }
  }, simplify = FALSE)
}

# shorter way of printing debug dumps
#' @export
print.mlr.dump = function(x, ...) {
  cat("<debug dump>\n")
  invisible(NULL)
}

