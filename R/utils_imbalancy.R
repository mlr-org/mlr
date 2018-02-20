
# sort classes wrt to size
getMinMaxClass = function(y) {
  tab = table(y)
  j.large = getMaxIndex(tab)
  ns = names(tab)
  min.names = ns[-j.large]
  max.name = ns[j.large]
  list(
    min.names = min.names,
    min.sizes = tab[-j.large],
    min.inds = setNames(lapply(min.names, function(z) which(y == z)), min.names),
    max.name = max.name,
    max.size = tab[j.large],
    max.inds = which(y == max.name)
  )
}

# @param y binaryclass vec
# @param rate resample rate
# @param cl main class to either down or upsample
# @param resample.other.class sample with replacement from other class?
# @return new y vec
#
# generates a new resampled y:
# a) class cl is either oversampled or downsampled, depending on rate
# b) the other binary class is either copied or bootstrapped (for variance)
sampleBinaryClass = function(y, rate, cl, resample.other.class) {
  inds1 = which(y == cl) # indices for class cl
  inds2 = setdiff(seq_along(y), inds1) # indices for other class
  newsize = round(length(inds1) * rate)
  # undersampling (rate < 1): reduce class1 by selecting newsize elements from it
  if (rate < 1) {
    newinds1 = sample(inds1, newsize, replace = FALSE)
  # oversampling (rate > 1): take existing inds and sample add. inds with repl.
  } else {
    newinds1 = c(inds1, sample(inds1, newsize - length(inds1), replace = TRUE))
  }
  # now either copy or bootstrap other class
  if (resample.other.class)
    newinds2 = sample(inds2, length(inds2), replace = TRUE)
  else
    newinds2 = inds2
  c(newinds1, newinds2)
}


sampleMultiClass = function(y, rate, cl) {
  inds1 = which(y == cl)
  newsize = round(length(inds1) * rate)
  if (rate < 1) {
    sample(inds1, newsize, replace = FALSE)
  } else {
    c(inds1, sample(inds1, newsize - length(inds1), replace = TRUE))
  }
}
