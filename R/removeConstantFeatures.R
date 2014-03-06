#' Remove constant features from a data set.
#' 
#' Constant features can lead to errors in some models. Given a \code{data.frame}, this function
#' removes all constant features. With the argument 'perc', there is a possibility to also remove
#' features for whom less than 'perc' percent of the observations differ from the mode value.
#' 
#' @param x [\code{\link{data.frame}}] or [\code{\link{SupervisedTask}}] \cr
#'   The data set or a task inlcuding it.
#' @param perc [\code{numeric}]\cr
#'   The percentage of a features' values (in [0, 1)) that must differ from the mode value. 
#'   Default is 0, which means only constant features with exactly one observed level are removed. 
#' @param safeCols [\code{character()}]\cr
#'   Name(s) of the columns which must not be deleted. (E.g. the target column - automaticaly added when provided a \code{task})
#' @param na.mode [\code{character}]\cr
#' \itemize{
#'   \item \code{ignore}: \code{NA}s will be ignored (lower value of differation)\cr
#'   \item \code{factor}: \code{NA}s will be seen as own value (higher value of differation) \cr
#'   \item \code{modus}: like factor but \code{NA}s can also be a modal value
#' } 
#' @return [\code{\link{data.frame}}] or [\code{\link{SupervisedTask}}]. The input \code{data.frame} or the task containing it without the columns containing the constant features.
#' @export
removeConstantFeatures = function(x, perc = 0, safeCols = NULL, na.mode = "ignore", show.info = TRUE, printed.features = 10L) {
  UseMethod("removeConstantFeatures")
} 

#' @method removeConstantFeatures data.frame
#' @S3method removeConstantFeatures data.frame
removeConstantFeatures.data.frame = function(x, perc = 0, safeCols = NULL, na.mode = "ignore", show.info = TRUE, printed.features = 10L) {
  checkArg(x, "data.frame")
  checkArg(perc, "numeric", lower=0, upper=1)
  stopifnot(perc!=1)
  
  if (ncol(x) == 0) 
    return(x)
  
  myMode <- function(x) {
    #thanks to http://stackoverflow.com/questions/2547402/
    if(na.mode!="modus")
      x = na.omit(x)
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  modi = lapply(x[, colnames(x) %nin% safeCols], myMode) #saves some calculations
  differ.ratio = sapply(names(modi), function(colName) {
    if(na.mode!="ignore")
      mean(!modi[[colName]] %equals% x[, colName])
    else{
      mean(!modi[[colName]] == x[, colName], na.rm=TRUE)
    }
  })
  select = differ.ratio > perc
  select = colnames(x) %in% c(safeCols, names(select[select])) #preserves order
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
removeConstantFeatures.SupervisedTask = function(x, perc = 0, safeCols = NULL, na.mode = "ignore", show.info = TRUE, printed.features = 10L) {
  checkArg(x, "SupervisedTask")
  safeCols = c(getTargetNames(x), safeCols)
  res = removeConstantFeatures(x = getTaskData(x), perc = perc, safeCols = safeCols, na.mode = na.mode, show.info, printed.features)
  changeData(task=x, data=res)
}

`%equals%` = function(x, y) {
  res = (x == y)  |  (is.na(x) & is.na(y))
  res[is.na(res)] <- FALSE
  res
}
