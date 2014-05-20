#' @title Calculates feature importance values.
#'
#' @description
#' Calculates numerical importance values for all features.
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
#' @return [\code{numeric}]. A named numeric vector that contains an importance value for each feature.
#' @seealso \code{\link{filterFeatures}} and \code{\link{makeFilterWrapper}}
#' @export
getFeatureFilterValues = function(obj, target, method="random.forest.importance") {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  checkArg(method, choices = c("linear.correlation", "rank.correlation", "information.gain",
                               "gain.ratio", "symmetrical.uncertainty", "chi.squared", 
                               "random.forest.importance", "relief", "oneR"))
  UseMethod("getFeatureFilterValues")
}

#' @export
getFeatureFilterValues.SupervisedTask = function(obj, target, method="random.forest.importance") {
  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (inherits(obj, "ClassifTask") || (obj$task.desc$n.feat["factors"] > 0L))
      stop("Method can only be applied for a regression task with numerical data!")
  }
  getFeatureFilterValues(getTaskData(obj), obj$task.desc$target, method)
}

#' @export
getFeatureFilterValues.data.frame = function(obj, target, method="random.forest.importance") {
  requirePackages("FSelector")
  checkArg(target, "character")
  fun = get(method, envir=getNamespace("FSelector"))
  f = paste(target, "~.")
  y = fun(as.formula(f), obj)
  vals = y[,1L]
  names(vals) = rownames(y)
  vals
  }
