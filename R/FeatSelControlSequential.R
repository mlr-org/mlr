#' @export
#' @rdname FeatSelControl
makeFeatSelControlSequential = function(same.resampling.instance=TRUE, method, alpha=0.01, beta=0.01,
  maxit=NA_integer_, max.features=NA_integer_) {

  makeFeatSelControl(same.resampling.instance=same.resampling.instance,
    maxit=maxit, max.features=max.features,
    method=method, alpha=alpha, beta=beta,
    cl="FeatSelControlSequential")
}
