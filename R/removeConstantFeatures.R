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
#' @param dont.rm [\code{character()}]\cr
#'   Names of the columns which must not be deleted.
#'   E.g. the target column - automatically added when provided a \code{task}.
#' @param na.mode [\code{character}]\cr
#' \itemize{
#'   \item \code{ignore}: \code{NA}s will be ignored (lower value of differation)\cr
#'   \item \code{factor}: \code{NA}s will be seen as own value (higher value of differation) \cr
#'   \item \code{modus}: like factor but \code{NA}s can also be a modal value
#' }
#' @return [\code{\link{data.frame}} | \code{\link{SupervisedTask}}].
#' @export
removeConstantFeatures = function(x, perc = 0, dont.rm = character(0), na.mode = "ignore",
  show.info = TRUE, printed.features = 10L) {

  checkArg(perc, "numeric", lower=0, upper=1, na.ok=FALSE)
  checkArg(dont.rm, "character", na.ok=FALSE)
  checkArg(na.mode, choices=c("ignore", "factor", "modus"))
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)
  UseMethod("removeConstantFeatures")
}

#' @method removeConstantFeatures data.frame
#' @S3method removeConstantFeatures data.frame
removeConstantFeatures.data.frame = function(x, perc = 0, dont.rm = character(0), na.mode = "ignore",
  show.info = TRUE, printed.features = 10L) {


  if (ncol(x) == 0)
    return(x)

  myMode <- function(x) {
    #thanks to http://stackoverflow.com/questions/2547402/
    if(na.mode!="modus")
      x = na.omit(x)
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  modi = lapply(x[, colnames(x) %nin% dont.rm], myMode) #saves some calculations
  differ.ratio = sapply(names(modi), function(colName) {
    if(na.mode!="ignore")
      mean(!modi[[colName]] %equals% x[, colName])
    else{
      mean(!modi[[colName]] == x[, colName], na.rm=TRUE)
    }
  })
  select = differ.ratio > perc
  select = colnames(x) %in% c(dont.rm, names(select[select])) #preserves order
  if(show.info){
    delcols = head(sort(differ.ratio[!select]), printed.features)
    messagef("Removing %i columns: %s",
             sum(!select),
             paste(names(delcols), round(delcols,2), collapse=", ", sep=": "))
  }
  x[, select, drop=FALSE]
}

#' @method removeConstantFeatures SupervisedTask
#' @S3method removeConstantFeatures SupervisedTask
removeConstantFeatures.SupervisedTask = function(x, perc = 0, dont.rm = NULL, na.mode = "ignore",
  show.info = TRUE, printed.features = 10L) {

  dont.rm = c(getTargetNames(x), dont.rm)
  res = removeConstantFeatures(x = getTaskData(x), perc = perc, dont.rm = dont.rm, na.mode = na.mode,
    show.info, printed.features)
  changeData(task=x, data=res)
}

`%equals%` = function(x, y) {
  res = (x == y)  |  (is.na(x) & is.na(y))
  res[is.na(res)] <- FALSE
  res
}
