
# sort classes wrt to size
getMinMaxClass = function(y) {
  tab = table(y)
  j.small = getMinIndex(tab)
  ns = names(tab)
  min.name = ns[j.small]
  max.name = ns[-j.small]
  list(
    min.name = min.name,
    min.size = tab[j.small],
    min.inds = which(y == min.name),
    max.name = max.name,
    max.size = tab[-j.small],
    max.inds = which(y == max.name)
  )
}

# resample one of the two classes cl
# @param y binaryclass vec
# @param rate resample rate
# @param cl classname to resample
# @param clreplace sample replacement of cl
# @param othreplace sample replacement of other cl
# @return new y vec
sampleBinaryClass = function(y, rate, cl, clreplace = TRUE, othreplace = TRUE) {
  inds1 = which(y == cl)
  inds2 = seq_along(y)[-inds1]
  newsize = round(length(inds1) * rate)
  # undersampling (rate < 1): sampling out of all inds of cl (but newsize <= max.size)
  # oversampling (rate > 1): take existing inds and sample add. inds with repl.  
  if (rate < 1) { 
    newinds1 = sample(inds1, newsize, replace = clreplace) 
  } else {
    newinds1 = c(inds1, sample(inds1, newsize - length(inds1), replace = clreplace))
  }
  newinds2 = sample(inds2, length(inds2), replace = othreplace)
  c(newinds1, newinds2)
}
