#' @export
#' @rdname FeatSelControl
makeFeatSelControlSequential = function(same.resampling.instance = TRUE, impute.val = NULL, method,
  alpha = 0.01, beta = -0.001, maxit = NA_integer_, max.features = NA_integer_,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = "default") {
  makeFeatSelControl(
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    maxit = maxit,
    max.features = max.features,
    method = method,
    alpha = alpha,
    beta = beta,
    tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args,
    log.fun = log.fun,
    cl = "FeatSelControlSequential"
  )
}
