#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance=TRUE, 
  maxit=as.integer(NA), max.features=as.integer(NA)) {
  
  makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, cl="FeatSelControlExhaustive")
}



