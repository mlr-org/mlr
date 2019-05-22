#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance = TRUE,
  maxit = 100L, max.features = NA_integer_, prob = 0.5, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = "default") {
  maxit = asCount(maxit, positive = TRUE)
  makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, max.features = max.features, prob = prob, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun, cl = "FeatSelControlRandom")
}
