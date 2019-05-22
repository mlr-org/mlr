#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance = TRUE, impute.val = NULL,
  maxit = NA_integer_, max.features = NA_integer_, comma = FALSE, mu = 10L, lambda,
  crossover.rate = 0.5, mutation.rate = 0.05, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = "default") {

  maxit = asCount(maxit, positive = TRUE)
  assertFlag(comma)
  mu = asCount(mu, positive = TRUE)
  if (missing(lambda)) {
    lambda = if (comma) 2L * mu else round(mu / 2L)
  } else {
    lam.low = if (comma) mu else 1L
    lambda = asInt(lambda, lower = lam.low)
  }
  assertNumber(crossover.rate, lower = 0, upper = 1)
  assertNumber(mutation.rate, lower = 0, upper = 1)

  makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, maxit = maxit, max.features = max.features, comma = comma,
    mu = mu, lambda = lambda, crossover.rate = crossover.rate,
    mutation.rate = mutation.rate,
    tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun,
    cl = "FeatSelControlGA")
}
