#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance=TRUE,
  maxit=100L, max.features=as.integer(NA), prob=0.5) {
  
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=FALSE)
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, prob=prob, cl="FeatSelControlRandom")
  return(ctrl)
}
