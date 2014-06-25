#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance = TRUE,
  maxit = 100L, max.features = NA_integer_, prob = 0.5) {

  maxit = convertInteger(maxit)
  assertInteger(maxit, len = 1L, lower = 1L, any.missing = FALSE)

  makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, max.features = max.features, prob = prob, cl = "FeatSelControlRandom")
}
