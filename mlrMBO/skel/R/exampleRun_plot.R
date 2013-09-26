#' Plot example run, either in 1D or 2D.
#'
#' The plot will show the following elements per iteration:
#' (a) Above plot
#' - The true objective function (solid line).
#' - The surrogate approximation, represented by its mean response (dotted line).
#' - Surrogate mean +- 1 standard deviation, from local uncertainty (dotted line).
#' (b) Lower plot
#' - Infill criterion.
#' 
#' In both plots the following elements are present
#' - Initial design points (black)
#' - Points from previous sequentail iteraions (green)
#' - Proposed point in current iteration. 
#'
#' @param x [\code{function}]\cr
#'   Objective function. 
#'   See
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
#' @param design.pch [\code{integer(1)}]\cr
#'   Three pch symbols used to display design points.
#'   Default is c(19,19,19), which are filled circular dots.
#' @param design.cols [\code{integer(1)}]\cr
#'   Three Colors for design points.
#'   First is for initial design, second is sequential
#'   points proposed in the past, third is for currently proposed points.
#'   Default are black, darkseagreen and tomato.
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
#' @return Nothing.
#' @method plot MBOExampleRun
#' @export
plot.MBOExampleRun = function(x, iters, pause=TRUE, 
  design.pch=c(19,19,19), design.cols=c("black", "darkseagreen", "tomato"), densregion=TRUE, 
  se.factor1=1, se.factor2=2, xlim, ylim, ...)  {
  
  # reset all changed pars on exit
  old.pars = par(c("mfrow", "mar", "oma"))
  on.exit(do.call(par, old.pars))
  
  niters = x$control$iters
  if (missing(iters)) {
    iters = seq_len(niters)
  } else {
    iters = convertIntegers(iters)
    checkArg(iters, "integer", min.len=1L, lower=1, upper=niters, na.ok=FALSE)
  }
  checkArg(pause, "logical", len=1L, na.ok=FALSE)
  checkArg(densregion, "logical", len=1L, na.ok=FALSE)
  checkArg(se.factor1, "numeric", len=1L, na.ok=FALSE)
  checkArg(se.factor2, "numeric", len=1L, na.ok=FALSE)
  #FIXME implement and document meaning for xlim, ylim for 2D plots
  if (!missing(xlim))
    checkArg(xlim, "numeric", len=2L, na.ok=FALSE)
  if (!missing(ylim))
    checkArg(ylim, "numeric", len=2L, na.ok=FALSE)
  
  if (x$n.params == 1L) {
    if (x$par.types %in% c("numeric", "numericvector")) {
      plotMBOExampleRun1DNumeric(x, pause=pause, iters=iters, design.pch=design.pch, design.cols=design.cols, 
        densregion=densregion, se.factor1=se.factor1, se.factor2=se.factor2, 
        xlim=xlim, ylim=ylim, ...)
    # FIXME: do we support "discretevector"
    } else if (x$par.types %in% c("discrete", "discretevector")) {
      plotMBOExampleRun1DDiscrete(x, pause=pause, iters=iters, xlim=xlim, ylim=ylim, ...)
    }
  } else if (x$n.params == 2L && all(x$par.types %in% c("numeric", "numericvector"))) {
    plotMBOExampleRun2DNumeric(x, pause=pause, iters=iters, design.pch=design.pch, design.cols=design.cols,
      xlim=xlim, ylim=ylim, ...)
  } else {
    stopf("Should not happen!")
  }
}
