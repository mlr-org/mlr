#' @export
#' @rdname FeatSelControl
makeFeatSelControlSequential = function(same.resampling.instance = TRUE, impute.val = Inf, method,
  alpha = 0.01, beta = -0.001, maxit = NA_integer_, max.features = NA_integer_) {

  makeFeatSelControl(
    cl = "FeatSelControlSequential",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    maxit = maxit,
    max.features = max.features,
    method = method,
    alpha = alpha,
    beta = beta
  )
}
