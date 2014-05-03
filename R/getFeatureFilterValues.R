#' @title Calculates feature filter values by using a numerical importance criterion.
#'
#' @description
#' Calculates numerical importance values for all features.
#' Thresholding of these values can be used to select \dQuote{useful} features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms.
#'
#' @param obj [\code{\link{data.frame}} | \code{\link{SupervisedTask}}]\cr
#'  The data set or task.
#' @param target [\code{character}]\cr
#'  Name of the column(s) specifying the response if you passed a \code{data.frame}.
#'  User input is ignored if you pass a task and \code{target} is automatically set.
#'  Never removed.   
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
#' @return [\code{numeric}]. A named numeric vector that contains an importance value
#'   for each feature.
#' @seealso \code{\link{makeFilterWrapper}}
#' @export
getFeatureFilterValues = function(obj, target, method="random.forest.importance") {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  UseMethod("getFeatureFilterValues")
}

#' @S3method getFeatureFilterValues data.frame
getFeatureFilterValues.data.frame = function(obj, target, method="random.forest.importance") {
  requirePackages("FSelector", "filterFeatures")
  if (!missing(target))
    checkArg(target, subset = colnames(obj)) 
  checkArg(method, choices=c("linear.correlation", "rank.correlation", "information.gain",
                             "gain.ratio", "symmetrical.uncertainty", "chi.squared", "random.forest.importance",
                             "relief", "oneR"))
  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (obj$task.desc$n.feat["factors"] > 0L)
      stop("Method can only be applied with numerical data!")
  }
  f = sprintf("%s ~.", target)
  fun = get(method, envir=getNamespace("FSelector"))
  y = fun(as.formula(f), obj)
  vals = y[,1L]
  names(vals) = rownames(y)
  return(vals)
}

#' @S3method getFeatureFilterValues SupervisedTask
getFeatureFilterValues.SupervisedTask = function(obj, target, method="random.forest.importance") {
  requirePackages("FSelector", "filterFeatures")
  if (!missing(target))
    stop("Do not pass 'target' when you pass a task!")
  checkArg(method, choices=c("linear.correlation", "rank.correlation", "information.gain",
                             "gain.ratio", "symmetrical.uncertainty", "chi.squared", "random.forest.importance",
                             "relief", "oneR"))
  if (method %in% c("linear.correlation", "rank.correlation")) {
    if (inherits(obj, "ClassifTask") || (obj$task.desc$n.feat["factors"] > 0L))
      stop("Method can only be applied for a regression task with numerical data!")
  }
  f = getTaskFormulaAsString(obj)
  data = getTaskData(obj)
  fun = get(method, envir=getNamespace("FSelector"))
  y = fun(as.formula(f), data)
  vals = y[,1L]
  names(vals) = rownames(y)
  return(vals)
}
