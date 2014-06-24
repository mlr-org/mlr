#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance = TRUE,
  maxit = NA_integer_, max.features = NA_integer_, comma = FALSE, mu = 10L, lambda, crossover.rate = 0.5, mutation.rate = 0.05) {

  maxit = convertInteger(maxit)
  assertInteger(maxit, len = 1L, lower = 1L, any.missing = FALSE)
  assertLogical(comma, len = 1L, any.missing = FALSE)
  mu = convertInteger(mu)
  assertInteger(mu, len = 1L, lower = 1L, any.missing = FALSE)
  if (missing(lambda))  {
    lambda = if (comma) 2L * mu else round(mu / 2L)
  } else {
    lambda = convertInteger(lambda)
    lam.low  = if (comma) mu else 1L
    assertInteger(lambda, len = 1L, lower = lam.low, any.missing = FALSE)
  }
  assertNumeric(crossover.rate, len = 1L, lower = 0, upper = 1, any.missing = FALSE)
  assertNumeric(mutation.rate, len = 1L, lower = 0, upper = 1, any.missing = FALSE)

  ctrl = makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, max.features = max.features,
    comma = comma, mu = mu, lambda = lambda,
    crossover.rate = crossover.rate, mutation.rate = mutation.rate,
		cl = "FeatSelControlGA")
  return(ctrl)
}
