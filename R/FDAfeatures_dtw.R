#' @export
getMultiChDtwFeatures = function(data, fd.features, ref) {
  ref = data[1, ]
  feat.list = namedList(names = names(fd.features))
  for(fdn in names(fd.features)){
    feat.list[[fdn]] = getUniDTWFeatures(data[, fd.features[[fdn]]], ref[, fd.features[[fdn]]])
  }
  as.data.frame(Reduce(cbind, x = feat.list))
}

getDtwDist = function(row, refs)
{
  requirePackages("dtw", default.method = "load")
  list.dist = lapply(refs, function(x) {
    res = dtw(row, x, step.pattern = asymmetric, keep =TRUE)
    res$distance  
  })
  unlist(list.dist)
}

#' @export
getUniDTWFeatures = function (data, refs = NULL){
  if(is.null(refs)) refs = c(data[1, ], data[2, ]) 
  m = apply(data, 1, function(x) getDtwDist(x, refs))
  t(as.matrix(m))
}
