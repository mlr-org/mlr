#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance = TRUE,
  maxit = NA_integer_, max.features = NA_integer_, tune.threshold = FALSE, log.fun = NULL) {

  makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, max.features = max.features, tune.threshold = tune.threshold,
    log.fun = log.fun, cl = "FeatSelControlExhaustive")
}
