#' Autoplot function for exampleRun objects.
#'
#' Serves for visualiziation of surrgate-based-optimization process for
#' functions with at most three parameters.
#'
#' @method autoplot MBOExampleRun
#' @export
autoplot.MBOexampleRun = function(x, iters, xlim, ylim, pause=TRUE, densregion=TRUE, ...) {
	# FIXME: check params
	n.params = x$n.params
	par.types = x$par.types
	if (n.params == 1) {
		autoplotExampleRun1d(x, iters, xlim, ylim, pause=pause, densregion=densregion, ...)
	} else if (n.params == 2) {
		autoplotExampleRun2d(x, iters=iters, xlim=xlim, ylim=ylim, pause=pause, ...)
	} else {
		stopf("Functions with greater than 3 parameters are not supported.")
	}
}