#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance=TRUE,
  maxit=NA_integer_, max.features=NA_integer_, comma = FALSE, mu=10L, lambda, crossover.rate=0.5, mutation.rate=0.05) {

  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=FALSE)
  checkArg(comma, "logical", len=1L, na.ok=FALSE)
  mu = convertInteger(mu)
  checkArg(mu, "integer", len=1L, lower=1L, na.ok=FALSE)
  if (missing(lambda))  {
    lambda = if (comma) 2L * mu else round(mu / 2L)
  } else {
    lambda = convertInteger(lambda)
    lam.low  = if (comma) mu else 1L
    checkArg(lambda, "integer", len=1L, lower=lam.low, na.ok=FALSE)
  }
  checkArg(crossover.rate, "numeric", len=1L, lower = 0, upper = 1, na.ok=FALSE)
  checkArg(mutation.rate, "numeric", len=1L, lower = 0, upper = 1, na.ok=FALSE)

  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance,
    maxit=maxit, max.features=max.features,
    comma=comma, mu=mu, lambda=lambda,
    crossover.rate=crossover.rate, mutation.rate=mutation.rate,
		cl="FeatSelControlGA")
  return(ctrl)
}
