getMultiChDtwFeatures = function(data, fd.features, ref) {
  ref = data[1, ]
  feat.list = namedList(names = names(fd.features))
  for(fdn in names(fd.features)){
    feat.list[[fdn]] = getUniDTWFeatures(data[, fd.features[[fdn]]], ref[, fd.features[[fdn]]])
  }
  as.data.frame(Reduce(cbind, x = feat.list))
}

getUniDTWFeatures = function (row, ref){ 
  a = dtw(t,r, step.pattern = asymmetric, keep =TRUE)                                            a$distance 
}