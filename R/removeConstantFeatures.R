#' @title Remove constant features from a data set.
#'
#' @description
#' Constant features can lead to errors in some models and obviously provide
#' no information in the training set that can be learned from.
#' With the argument \dQuote{perc}, there is a possibility to also remove
#' features for which less than \dQuote{perc} percent of the observations
#' differ from the mode value.
#'
#' @template arg_taskdf
#' @param perc (`numeric(1)`)\cr
#'   The percentage of a feature values in [0, 1) that must differ from the mode value.
#'   Default is 0, which means only constant features with exactly one observed level are removed.
#' @param dont.rm ([character])\cr
#'   Names of the columns which must not be deleted.
#'   Default is no columns.
#' @param na.ignore (`logical(1)`)\cr
#'   Should NAs be ignored in the percentage calculation?
#'   (Or should they be treated as a single, extra level in the percentage calculation?)
#'   Note that if the feature has only missing values, it is always removed.
#'   Default is `FALSE`.
#' @param tol (`numeric(1)`)\cr
#'   Numerical tolerance to treat two numbers as equal.
#'   Variables stored as `double` will get rounded accordingly before computing the mode.
#'   Default is `sqrt(.Maschine$double.eps)`.
#' @template arg_showinfo
#' @template ret_taskdf
#' @export
#' @family eda_and_preprocess
removeConstantFeatures = function(obj, perc = 0, dont.rm = character(0L), na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = getMlrOption("show.info")) {
  UseMethod("removeConstantFeatures")
}

#' @export
removeConstantFeatures.Task = function(obj, perc = 0, dont.rm = character(0L), na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = getMlrOption("show.info")) {
  assertCharacter(dont.rm)
  dont.rm = union(dont.rm, getTaskTargetNames(obj))
  data = removeConstantFeatures(getTaskData(obj, functionals.as = "matrix"), perc = perc, dont.rm = dont.rm, na.ignore = na.ignore, tol = tol, show.info = show.info)
  changeData(task = obj, data = data)
}

#' @export
removeConstantFeatures.data.frame = function(obj, perc = 0, dont.rm = character(0L), na.ignore = FALSE, tol = .Machine$double.eps^.5, show.info = getMlrOption("show.info")) {

  assertNumber(perc, lower = 0, upper = 1)
  assertSubset(dont.rm, choices = names(obj))
  assertFlag(na.ignore)
  assertNumber(tol, lower = 0)
  assertFlag(show.info)

  if (any(!dim(obj))) {
    return(obj)
  }

  isEqual = function(x, y) {
    res = (x == y) | (is.na(x) & is.na(y))
    replace(res, is.na(res), FALSE)
  }
  digits = ceiling(log10(1 / tol))
  cns = setdiff(colnames(obj), dont.rm)
  ratio = vnapply(obj[cns], function(x) {
    if (allMissing(x)) {
      return(0)
    }
    if (is.double(x)) {
      x = round(x, digits = digits)
    }
    m = computeMode(x, na.rm = na.ignore, ties.method = "first")
    if (na.ignore) {
      mean(m != x, na.rm = TRUE)
    } else {
      mean(!isEqual(x, m))
    }
  }, use.names = FALSE)

  dropcols = cns[ratio <= perc]
  if (show.info && length(dropcols)) {
    messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
  }
  dropNamed(obj, dropcols)
}
