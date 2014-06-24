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
#' @param target [\code{character}]\cr
#'   Name of the column(s) specifying the response if you passed a \code{data.frame}.
#'   User input is ignored if you pass a task and \code{target} is automatically set.
#'   Never removed.
#' @param perc [\code{numeric(1)}]\cr
#'   The percentage of a feature values in [0, 1) that must differ from the mode value.
#'   Default is 0, which means only constant features with exactly one observed level are removed.
#' @param dont.rm [\code{character}]\cr
#'   Names of the columns which must not be deleted.
#'   Default is no columns.
#' @param na.ignore [\code{logical(1)}]\cr
#'   Should NAs be ignored in the percentage calculation?
#'   (Or should they be treated as a single, extra level in the percentage calculation?)
#'   Default is \code{FALSE}.
#' @param tol [\code{numeric(1)}]\cr
#'   Numerical tolerance to treat two numbers as equal.
#'   Variables stored as \code{double} will get rounded accordingly before computing the mode.
#'   Default is \code{sqrt(.Maschine$double.eps)}.
#' @template arg_showinfo
#' @return [\code{\link{data.frame}} | \code{\link{SupervisedTask}}].
#' @export
removeConstantFeatures = function(x, target, perc = 0, dont.rm = character(0L),
  na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = getMlrOption("show.info")) {

  checkArg(x, c("data.frame", "SupervisedTask"))
  assertNumeric(perc, len = 1L, lower = 0, upper = 1, any.missing = FALSE)
  assertCharacter(dont.rm, any.missing = FALSE)
  assertLogical(na.ignore, len = 1L, any.missing = FALSE)
  assertLogical(show.info, len = 1L, any.missing = FALSE)
  UseMethod("removeConstantFeatures")
}

#' @method removeConstantFeatures data.frame
#' @export
removeConstantFeatures.data.frame = function(x, target, perc = 0, dont.rm = character(0L),
  na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = TRUE) {

  assertSubset(dont.rm, choices = colnames(x))
  if (!missing(target)) {
    assertSubset(target, choices = colnames(x))
    dont.rm = union(dont.rm, target)
  }

  if (any(!dim(x)))
    return(x)

  isEqual = function(x, y) {
    res = (x == y) | (is.na(x) & is.na(y))
    replace(res, is.na(res), FALSE)
  }

  digits = ceiling(log10(1 / tol))

  cns = setdiff(colnames(x), dont.rm)
  ratio = vnapply(x[, cns, drop = FALSE], function(x) {
    if (is.double(x))
      x = round(x, digits = digits)
    m = computeMode(x, na.rm = na.ignore)
    if (na.ignore) {
      mean(m != x, na.rm = TRUE)
    } else {
      mean(!isEqual(x, m))
    }
  }, use.names = FALSE)

  dropcols = cns[ratio <= perc]
  if (show.info && length(dropcols))
    messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
  dropNamed(x, dropcols)
}

#' @method removeConstantFeatures SupervisedTask
#' @export
removeConstantFeatures.SupervisedTask = function(x, target, perc = 0, dont.rm = character(0L),
  na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = TRUE) {

  if (!missing(target))
    stop("Do not pass 'target' when you pass a task!")
  res = removeConstantFeatures(getTaskData(x), getTargetNames(x), perc, dont.rm, na.ignore, tol, show.info)
  changeData(task = x, data = res, costs = x$env$costs)
}
