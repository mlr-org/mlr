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
#' @param method [\code{character(1)}]\cr
#'   Filter method. Available are:\cr
#'   \dQuote{linear.correlation}: Pearson's correlation for regression with numerical data\cr 
#'   \dQuote{rank.correlation}: Spearman's correlation for regression with numerical data\cr 
#'   \dQuote{information.gain}: Entropy-based information gain for classification and 
#'   regression of mixed feature sets\cr
#'   \dQuote{gain.ratio}: Entropy-based gain ratio for classification and regression of 
#'   mixed feature sets\cr 
#'   \dQuote{symmetrical.uncertainty}: Entropy-based symmetrical uncertainty for 
#'   classification and regression of mixed feature sets\cr 
#'   \dQuote{chi.squared}: Finds weights of mixed feature sets using chi-squared test for 
#'   classification and regression\cr
#'   \dQuote{random.forest.importance}: Finds weights of mixed feature sets using 
#'   RandomForest algorithm for classification and regression\cr 
#'   \dQuote{relief}: Finds weights of mixed feature sets using distances between instances 
#'   for classification and regression\cr 
#'   \dQuote{oneR}: Finds weights of mixed feature sets using association rules for 
#'   classification and regression\cr 
#'   Default is \dQuote{random.forest.importance}.
#' @param threshold [\code{numeric(1)}]\cr
#'   Information value as to be greater then the threshold. Default is 0.
#' @param n [\code{integer(1)}]\cr
#'   Number of features ordered by the information value to select.
#' @param percentage [\code{numeric(1)}]\cr
#'   Alternatively to \code{n} you can give a relative number of features.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as \code{obj}.
#' @seealso \code{\link{makeFilterWrapper}}
#' @export
filterFeatures = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  checkFilterArguments(method, threshold, n, percentage)
  UseMethod("filterFeatures")
}

#' @export
filterFeatures.SupervisedTask = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  d = filterFeatures(getTaskData(obj), obj$task.desc$target, method, threshold, n, percentage)
  changeData(obj, d)
}

#' @export
filterFeatures.data.frame = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  requirePackages("FSelector")
  checkArg(target, "character")
  feat.importance = getFeatureFilterValues(obj, target, method)
  if(threshold <= 0 && any(feat.importance<threshold)){
    warningf("The threshold was set to %d but they are observed feature importance values below 
          that value generetad by %s .", threshold, method)
  }
  if(isNotSet(n))
    n = length(feat.importance)
  if(!isNotSet(percentage))
    n = round(percentage * length(feat.importance))
  feats = names(feat.importance[feat.importance >= threshold])
  feats = head(feats[order(feat.importance, decreasing=TRUE)], n)
  obj[, colnames(obj) %in% c(feats, target), drop = FALSE] #preserves order!
  }

checkFilterArguments = function(method, threshold, n, percentage) {
  if(!isNotSet(method)) {
    checkArg(method, choices = filter.methods)
  }  
  if(!isNotSet(threshold)) {
    checkArg(threshold, "numeric", len = 1L, na.ok = FALSE)
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

filter.methods = c("linear.correlation", "rank.correlation", "information.gain",
                   "gain.ratio", "symmetrical.uncertainty", "chi.squared", 
                   "random.forest.importance", "relief", "oneR")
