#' @title Returns the filtered features.
#'
#' @description
#' Returns the selected Features according to their importance.
#' Features with an importance value lower than threshold will be left out 
#' regardless of the given \code{n}.
#'
#' @param obj [\code{\link{data.frame}} | \code{\link{ClassifTask}}]\cr
#'  The data set or task.
#' @param target [\code{character}]\cr
#'  Name of the column(s) specifying the response if you passed a \code{data.frame}.
#'  User input is ignored if you pass a task and \code{target} is automatically set.
#'  Never removed.   
#' @param feat.importance [\code{numeric}]\cr
#'   Result of \code{\link{getFeatureFilterValues}}.
#' @param n [\code{integer(1)}]\cr
#'   Number of features ordered by the information value to select.
#' @param threshold [\code{numeric(1)}]\cr
#'   Information value as to be greater then the threshold. Default is 0.
#' @return [\code{data.frame} | \code{\link{ClassifTask}}]. Same type as \code{obj}.
#' @export
filterFeatures = function(obj, target, feat.importance, n, threshold = 0) {
  checkArg(obj, c("data.frame", "ClassifTask"))
  UseMethod("filterFeatures")
}

#' @S3method filterFeatures data.frame
filterFeatures.data.frame = function(obj, target, feat.importance, n, threshold = 0) {
  if (!missing(target))
    checkArg(target, subset = colnames(obj))   
  checkArg(feat.importance, "numeric", na.ok=FALSE)
  checkArg(threshold, "numeric", len=1L, na.ok=FALSE)
  if (missing(n))
    n = length(feat.importance)
  checkArg(n, "integer", len=1L, lower=1L, na.ok=FALSE)
  
  feats = feat.importance[feat.importance > threshold]
  feats = head(feats[order(feats, decreasing=TRUE)], n)
  results = names(feats)
  # results = makeFeatSelResult(learner=NA, control=NA, x=names(feats), y=feats, opt.path=NA)
  subset(obj, select = c(results, target))
}

#' @S3method filterFeatures ClassifTask
filterFeatures.ClassifTask = function(obj, target, feat.importance, n, threshold = 0) {
  if (!missing(target))
    stop("Do not pass 'target' when you pass a task!")
  checkArg(feat.importance, "numeric", na.ok=FALSE)
  checkArg(threshold, "numeric", len=1L, na.ok=FALSE)
  if (missing(n))
    n = length(feat.importance)
  checkArg(n, "integer", len=1L, lower=1L, na.ok=FALSE)
  
  df = getTaskData(obj)
  feats = feat.importance[feat.importance > threshold]
  feats = head(feats[order(feats, decreasing=TRUE)], n)
  results = names(feats)
  subsetTask(obj, features = results)
}
