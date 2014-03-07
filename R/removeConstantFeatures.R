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
#' @return [\code{\link{data.frame}} | \code{\link{SupervisedTask}}].
#' @export
removeConstantFeatures = function(x, perc = 0, dont.rm = character(0), na.mode = "na.rm",
  show.info = TRUE) {

  checkArg(x, c("data.frame", "SupervisedTask"))
  checkArg(perc, "numeric", len=1L, lower=0, upper=1, na.ok=FALSE)
  checkArg(dont.rm, "character", na.ok=FALSE)
  checkArg(na.mode, choices=c("na.rm", "single", "distinct"))
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  UseMethod("removeConstantFeatures")
}

#' @method removeConstantFeatures data.frame
#' @S3method removeConstantFeatures data.frame
removeConstantFeatures.data.frame = function(x, perc = 0, dont.rm = character(0), na.mode = "na.rm",
  show.info = TRUE) {

  checkArg(dont.rm, subset=colnames(x))
  if (ncol(x) == 0)
    return(x)

  cns = setdiff(colnames(x), dont.rm)
  # compute modes for cols
  modi = lapply(x[, cns], computeMode, na.rm = (na.mode != "single"))
  # compute how many entries differ from mode val
  differ.ratio = sapply(cns, function(cn) {
    if (na.mode != "na.rm")
      mean(!modi[[cn]] %equals2% x[, cn])
    else {
      mean(modi[[cn]] != x[, cn], na.rm=TRUE)
    }
  })
  dropcols = cns[differ.ratio <= perc]
  if (show.info) {
    messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
  }
  dropNamed(x, dropcols)
}

#' @method removeConstantFeatures SupervisedTask
#' @S3method removeConstantFeatures SupervisedTask
removeConstantFeatures.SupervisedTask = function(x, perc = 0, dont.rm = character(0), na.mode = "na.rm",
  show.info = TRUE) {

  dont.rm = union(getTargetNames(x), dont.rm)
  res = removeConstantFeatures(getTaskData(x), perc, dont.rm, na.mode, show.info)
  changeData(task=x, data=res)
}


# checks which elements are equal
# NA == NA, NA != 1, 1 == 1, 1 != 2
`%equals2%` = function(x, y) {
  res = (x == y)  |  (is.na(x) & is.na(y))
  res[is.na(res)] <- FALSE
  res
}
