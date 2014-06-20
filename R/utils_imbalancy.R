
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
sampleBinaryClass = function(y, rate, cl, replace) {
  z = getMinMaxClass(y)
  inds1 = switch(cl, min = z$min.inds, max = z$max.inds)
  inds2 = switch(cl, min = z$max.inds, max = z$min.inds)
  newsize = round(length(inds1) * rate)
  newinds = sample(inds1, newsize, replace = replace)
  c(newinds, inds2)
}

