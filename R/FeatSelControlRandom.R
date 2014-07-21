#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance = TRUE,
  maxit = 100L, max.features = NA_integer_, prob = 0.5) {

  maxit = asCount(maxit, positive = TRUE)
  makeFeatSelControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, max.features = max.features, prob = prob, cl = "FeatSelControlRandom")
}
