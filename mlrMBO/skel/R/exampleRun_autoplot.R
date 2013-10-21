#' Plot example run, either in 1D or 2D.
#'
#' The plot will show the following elements per iteration:
#' - The true objective function (solid line).
#' - The surrogate approximation, represented by its mean response.
#' - Surrogate mean +- 1 standard deviation, from local uncertainty.
#' - Infill criterion.
#' 
#' In both plots the following elements are present
#' - Initial design points
#' - Points from previous sequentail iteraions
#' - Proposed point in current iteration. 
#'
#' @param x [\code{function}]\cr
#'   Objective function. 
#' @param ... [\code{\link[mlr]{Learner}}]\cr
#'   Surrogate model used for the optimization of \code{fun}.
#'   Default is mlr learner \dQuote{regr.km}, which is kriging from package
#'   DiceKriging. \code{nugget.estim} is set to \code{TRUE} depending on whether we have 
#'   noisy observations or not.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{x} to display.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Pause after each iteration?
#'   Default is \code{TRUE}.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded by the density of the 
#'   posterior distribution?
#'   Looks nice, but is currently pretty slow. You might wnat to
#'   disable this if you want to do stuff more interactively.
#'   Default is \code{TRUE}.
#' @param se.factor1 [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean respone \code{yhat(x) +- se.factor1 * se(x)}
#'   is plotted above and below. 
#'   Default is 1.
#' @param se.factor2 [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation and
#'   \code{densregion=TRUE}, the background is shaded 
#'   in the area \code{yhat(x) +- se.factor2 * se(x)}.
#'   Default is 2.
#' @param xlim [\code{numeric(2)}]\cr
#'   For 1D: \code{xlim} parameter for first and second plot. 
#'   Default is range of x-values evaluated in run object \code{x}.
#' @param ylim [\code{numeric(2)}]\cr
#'   For 1D: \code{ylim} parameter for first plot, for the second plot \code{ylim} is always set
#'   automatically, depending on the range of the evaluated infill criterion. 
#'   Default for the first plot is a heuristic to have the true function 
#'   and \code{yhat(x) +- se.factor2 * se(x)} both in the plot. Note that this heuristic might 
#'   change the \code{ylim} setting between plot iterations.
#' @param ... [\code{list}]\cr
#'   Further parameters. 
#' @return Nothing.
#' @S3method autoplot MBOExampleRun
#' @export
autoplot.MBOExampleRun = function(x, iters, pause=TRUE, densregion=TRUE, 
	se.factor1=1, se.factor2=2, xlim, ylim, point.size=3, trafo=NULL, ...) {
	  iters.max = x$control$iters
  	if (missing(iters)) {
    	iters = seq_len(iters.max)
  	} else {
    	iters = convertIntegers(iters)
    	checkArg(iters, "integer", min.len=1L, lower=1, upper=iters.max, na.ok=FALSE)
  	}
  	checkArg(pause, "logical", len=1L, na.ok=FALSE)
  	checkArg(densregion, "logical", len=1L, na.ok=FALSE)
  	checkArg(se.factor1, "numeric", len=1L, na.ok=FALSE)
 	  checkArg(se.factor2, "numeric", len=1L, na.ok=FALSE)
    if (!is.null(trafo)) {
      # if single function provided, apply it to all plots
      if (c("MBOTrafoFunction") %in% class(trafo)) {
        trafo = list("y" = trafo, "yhat" = trafo, "crit" = trafo, "se" = trafo)
      } else {
        # otherwise check if all elements are of an appropriate type
        lapply(trafo, function(t) if(!is.null(t)) checkArg(t, "MBOTrafoFunction"))

        trafo.defaults = list("y" = NULL, "yhat" = NULL, "crit" = NULL, "se" = NULL)
        trafo.defaults[names(trafo)] = trafo
        trafo = trafo.defaults 
      }
    }
  	#FIXME implement and document meaning for xlim, ylim for 2D plots
 	  if (!missing(xlim))
    	checkArg(xlim, "numeric", len=2L, na.ok=FALSE)
  	if (!missing(ylim))
    	checkArg(ylim, "numeric", len=2L, na.ok=FALSE)

	n.params = x$n.params
	par.types = x$par.types
	if (n.params == 1) {
		autoplotExampleRun1d(x, iters, xlim, ylim, pause=pause, point.size=point.size, densregion=densregion, ...)
	} else if (n.params == 2) {
		autoplotExampleRun2d(x, iters=iters, xlim=xlim, ylim=ylim, 
      pause=pause, point.size=point.size, trafo=trafo, ...)
	} else {
		stopf("Functions with greater than 3 parameters are not supported.")
	}
}