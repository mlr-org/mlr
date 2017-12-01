##' @include TuneControl.R
#roxygen()
#
##' Control structure for MI-ES tuning. 
##' @exportClass TuneControlMies
##' @seealso \code{\link{makeTuneControlMies}}
#
#setClass(
#  "TuneControlMies",
#  contains = "TuneControl"
#)
#
#
##' Create control structure for MI-ES tuning. 
##' 
##' @title Control for MI-ES tuning. 
##' @param path [\code{logical(1)}]\cr
##'   Should optimization path be saved? Default is TRUE.
##' @param same.resampling.instance [\code{logical(1)}]\cr
##'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
##' @param ... Further control parameters passed to the \code{control} argument of \code{\link[mies]{mies}}.
##' @return [\code{\linkS4class{TuneControlMies}}].
##' @export
#makeTuneControlMies = function(path=TRUE, same.resampling.instance=TRUE, ...) {
#  checkArg(path, "logical", len=1, na.ok=FALSE)
#  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
#  new("TuneControlMies", path=path, same.resampling.instance=same.resampling.instance, start=list(), ...)
#}