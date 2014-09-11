
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
# clreplace - replacement of class in argument cl, othreplace - replacement of other class in each case
sampleBinaryClass = function(y, rate, cl, clreplace = TRUE, othreplace = TRUE, bagging = FALSE) {
  z = getMinMaxClass(y)
  inds1 = switch(cl, min = z$min.inds, max = z$max.inds)
  inds2 = switch(cl, min = z$max.inds, max = z$min.inds)
  newsize = round(length(inds1) * rate)
  # undersampling (cl = "max"): sampling of all inds of maxClass (but newsize <= max.size)
  # oversampling (cl = "min" and bagging = FALSE): take existing inds and sample add. inds with repl.
  # overbagging (cl = "min" and bagging = TRUE): sampling of all inds of minClass  
  newinds1 = if( (cl == "max") || bagging ) { sample(inds1, newsize, replace = clreplace) 
  } else { c(inds1, sample(inds1, newsize-length(inds1), replace = clreplace)) }
  newinds2 = sample(inds2, length(inds2), replace = othreplace)
  c(newinds1, newinds2)
}
