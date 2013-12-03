#FIXME for which tasks do the filters work?

#' Filter features by using a numerical importance criterion.
#' Calculates numerical importance values for all features. 
#' Thresholding of these values can be used to select \dQuote{useful} features.
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.  
#' @param method [\code{character(1)}]\cr
#'   Filter method. Available are:
#'   \dQuote{linear.correlation}, \dQuote{rank.correlation}, \dQuote{information.gain}, 
#'   \dQuote{gain.ratio}, \dQuote{symmetrical.uncertainty}, \dQuote{chi.squared}, 
#'   \dQuote{random.forest.importance}, \dQuote{relief}, \dQuote{oneR}
#'   Default is \dQuote{random.forest.importance}.
#' @return [\code{numeric}]. A named numeric vector that contains an importance value 
#'   for each feature.
#' @export
filterFeatures = function(task, method="random.forest.importance") {
  requirePackages("FSelector", "filterFeatures")
  checkArg(task, "SupervisedTask") 
  checkArg(method, choices=c("linear.correlation", "rank.correlation", "information.gain", 
    "gain.ratio", "symmetrical.uncertainty", "chi.squared", "random.forest.importance", 
    "relief", "oneR"))
  tn = task$task.desc$target
  f = getTaskFormulaAsString(task)
  data = getTaskData(task)
  fun = get(method, envir=getNamespace("FSelector"))
  y = fun(as.formula(f), data)  
  vals = y[,1]
  names(vals) = rownames(y)
  return(vals)
} 
 

