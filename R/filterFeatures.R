#' @title Filter features by thresholding importance values.
#'
#' @description
#' Selects the features by importance values and according to a given threashold.
#' 
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param feat.importance [\code{numeric}]\cr
#'   Importance values for all features. See \code{\link{getFeatureFilterValues}}.
#' @param threshold [\code{numeric(1)}]\cr
#'   Information value as to be greater then the threshold. Default is 0.
#' @param n [\code{integer(1)}]\cr
#'   Number of features ordered by the information value to select.
#' @param percentage [\code{numeric(1)}]\cr
#'   Alternatively to \code{n} you can give a relative number of features.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as \code{obj}.
#' @seealso \code{\link{makeFilterWrapper}}
#' @export
filterFeatures = function(obj, target, feat.importance, threshold = 0, n = NULL, percentage = NULL) {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  checkArg(feat.importance, "numeric")
  checkFilterArguments(threshold, n, percentage)
  UseMethod("filterFeatures")
}

#' @export
filterFeatures.SupervisedTask = function(obj, target, feat.importance, threshold = 0, n = NULL, percentage = NULL) {
  d = filterFeatures(getTaskData(obj), obj$task.desc$target, feat.importance, threshold, n, percentage)
  changeData(obj, d)
}

#' @export
filterFeatures.data.frame = function(obj, target, feat.importance, threshold = 0, n = NULL, percentage = NULL) {
  requirePackages("FSelector")
  checkArg(target, "character")
  if(isNotSet(n))
    n = length(feat.importance)
  if(!isNotSet(percentage))
    n = round(percentage * length(feat.importance))
  feats = names(feat.importance[feat.importance >= threshold])
  feats = head(feats[order(feat.importance, decreasing=TRUE)], n)
  obj[, colnames(obj) %in% c(feats, target), drop = FALSE] #preserves order!
  }

checkFilterArguments = function(threshold, n, percentage) {
  if(!isNotSet(threshold)) {
    checkArg(threshold, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  }
  if(!isNotSet(n)) {
    checkArg(convertInteger(n), "integer", len = 1L, na.ok = FALSE, lower = 0)
  }
  if(!isNotSet(percentage)) {
    checkArg(percentage, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  }
  if(!isNotSet(n) && !isNotSet(percentage)) {
    stop("You can only filter n OR a percentage of features!")
  }
  if(isNotSet(threshold) && isNotSet(n) && isNotSet(percentage))
    stop("You have to provide at least a threshold, a n or a percentage!")
}

isNotSet = function(x) {
  missing(x) || is.null(x)
}
