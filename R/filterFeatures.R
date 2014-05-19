#' @title Filter features by using a numerical importance criterion.
#'
#' @description
#' Calculates numerical importance values for all features and selects the Features
#' according to a given threashold.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
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
filter = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  checkFilterArguments(method, threshold, n, percentage)
  UseMethod("filter")
}

filter.SupervisedTask = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (inherits(obj, "ClassifTask") || (obj$task.desc$n.feat["factors"] > 0L))
      stop("Method can only be applied for a regression task with numerical data!")
  }
  d = filter(getTaskData(obj), obj$task.desc$target, method, threshold, n, percentage)
  changeData(obj, d)
}

filter.data.frame = function(obj, target, method="random.forest.importance", threshold = 0, n = NULL, percentage = NULL) {
  requirePackages("FSelector", "filterFeatures")
  checkArg(target, "character")
  if(isNotSet(threshold))
    threshold = 0 #FIXME: Are we sure that 0 is the lowest? See warnf below.
  if(isNotSet(n))
    n = ncol(obj)
  if(!isNotSet(percentage))
    n = round(percentage * ncol(obj))
  fun = get(method, envir=getNamespace("FSelector"))
  f = paste(target, "~.")
  y = fun(as.formula(f), obj)
  vals = y[,1L]
  if(threshold <= 0 && any(vals<threshold)){
    warnf("The threshold was set to %d but they are observed feature importance values below 
          that value generetad by %s .", threshold, method)
  }
  names(vals) = rownames(y)
  feats = names(vals[vals >= threshold])
  feats = head(feats, n)
  obj[, colnames(obj) %in% c(feats, target), drop = FALSE] #preserves order!
}

checkFilterArguments = function(method, threshold, n, percentage) {
  checkArg(method, choices = filter.methods)
  if(!isNotSet(threshold)) {
    checkArg(threshold, "numeric", len=1L, na.ok=FALSE, lower=0)
  }
  if(!isNotSet(n)) {
    checkArg(convertInteger(n), "integer", len=1L, na.ok=FALSE, lower=0)
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
    "gain.ratio", "symmetrical.uncertainty", "chi.squared", "random.forest.importance",
    "relief", "oneR")

isNotSet = function(x) {
  missing(x) || is.null(x)
}
