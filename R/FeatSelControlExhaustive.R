#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance = TRUE, impute.val = Inf,
  maxit = NA_integer_, max.features = NA_integer_) {

  makeFeatSelControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    maxit = maxit, max.features = max.features, cl = "FeatSelControlExhaustive")
}
