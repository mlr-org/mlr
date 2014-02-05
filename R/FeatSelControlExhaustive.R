#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance=TRUE,
  maxit=NA_integer_, max.features=NA_integer_) {
  makeFeatSelControl(same.resampling.instance=same.resampling.instance,
    maxit=maxit, max.features=max.features, cl="FeatSelControlExhaustive")
}
