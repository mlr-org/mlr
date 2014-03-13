#' Remove constant features from a data set.
#'
#' Constant features can lead to errors in some models and obviously provide
#' no information in the training set that can be learned from.
#' With the argument \dQuote{perc}, there is a possibility to also remove
#' features for which less than \dQuote{perc} percent of the observations
#' differ from the mode value.
#'
#' @param x [\code{\link{data.frame}} | \code{\link{SupervisedTask}}]\cr
#'   The data set or task.
#' @param perc [\code{numeric(1)}]\cr
#'   The percentage of a feature values in [0, 1) that must differ from the mode value.
#'   Default is 0, which means only constant features with exactly one observed level are removed.
#' @param dont.rm [\code{character}]\cr
#'   Names of the columns which must not be deleted.
#'   Default is no columns.
#'   Note that for a task the target column is always added to these so it is never removed.
#' @param na.mode [\code{character(1)}]\cr
#' \itemize{
#'   \item \code{na.rm}: \code{NA}s will be ignored in the percentage calculation.\cr
#'   \item \code{single}: NAs will be seen as a single, extra level in the percentage calculation.\cr
#'   \item \code{distinct}: \code{NA}s will be seen as multiple extra levels in the percentage calculation.\cr
#' }
#' @param tol [\code{numeric}]\cr
#'   Numerical tolerance to treat two numbers as equal.
#'   Variables stored as \code{double} will get rounded accordingly before computing the mode.
#'   Default is \code{sqrt(.Maschine$double.eps)}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{FALSE}.
#' @return [\code{\link{data.frame}} | \code{\link{SupervisedTask}}].
#' @export
removeConstantFeatures = function(x, perc = 0, dont.rm = character(0L),
  na.mode = "na.rm", tol = .Machine$double.eps^.5, show.info = FALSE) {

  checkArg(x, c("data.frame", "SupervisedTask"))
  checkArg(perc, "numeric", len=1L, lower=0, upper=1, na.ok=FALSE)
  checkArg(dont.rm, "character", na.ok=FALSE)
  checkArg(na.mode, choices=c("na.rm", "single", "distinct"))
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  UseMethod("removeConstantFeatures")
}

#' @method removeConstantFeatures data.frame
#' @S3method removeConstantFeatures data.frame
removeConstantFeatures.data.frame = function(x, perc = 0, dont.rm = character(0L),
  na.mode = "na.rm", tol = .Machine$double.eps^.5, show.info = FALSE) {
  isEqual = function(x, y) {
    res = (x == y) | (is.na(x) & is.na(y))
    replace(res, is.na(res), FALSE)
  }

  checkArg(dont.rm, subset=colnames(x))
  if (any(!dim(x)))
    return(x)

  digits = ceiling(log10(1/tol))

  cns = setdiff(colnames(x), dont.rm)
  ratio = vnapply(x[, cns, drop=FALSE], function(x) {
    if (is.double(x))
      x = round(x, digits=digits)
    m = computeMode(x, na.rm = (na.mode != "single"))
    if (na.mode != "na.rm") {
      mean(!isEqual(x, m))
    } else {
      mean(m != x, na.rm=TRUE)
    }
  }, use.names = FALSE)

  dropcols = cns[ratio <= perc]
  if (show.info && length(dropcols))
    messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
  dropNamed(x, dropcols)
}

#' @method removeConstantFeatures SupervisedTask
#' @S3method removeConstantFeatures SupervisedTask
removeConstantFeatures.SupervisedTask = function(x, perc = 0, dont.rm = character(0L),
  na.mode = "na.rm", tol = .Machine$double.eps^.5, show.info = FALSE) {

  dont.rm = union(getTargetNames(x), dont.rm)
  res = removeConstantFeatures(getTaskData(x), perc, dont.rm, na.mode, tol, show.info)
  changeData(task=x, data=res)
}
