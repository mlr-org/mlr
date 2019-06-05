#' @param maxit (`integer(1)`)\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlRandom = function(same.resampling.instance = TRUE,
  maxit = 100L, log.fun = "default", final.dw.perc = NULL, budget = NULL) {
  if (is.null(budget)) {
    budget = maxit
  } else if (is.null(maxit)) {
    maxit = budget
  } else if (budget != maxit) {
    stopf("The parameters budget (%i) and maxit (%i) differ.", budget, maxit)
  }
  maxit = asCount(maxit)
  budget = asCount(budget)

  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, cl = "TuneMultiCritControlRandom")
}
