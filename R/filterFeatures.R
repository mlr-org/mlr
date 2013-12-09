#' Filter features by using a numerical importance criterion.
#'
#' Calculates numerical importance values for all features. 
#' Thresholding of these values can be used to select \dQuote{useful} features.
#' Look at package \code{\link[FSelector]{FSelector}} for details on the filter algorithms. 
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
#'   @seealso \code{\link{getFilteredFeatures}}
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



#FIXME: we should rather allow 
# filter: task, method, threshold --> new task
# and
# filter: data.frame, target, method, threshold --> data.frame
# see oversample and impute for this


# Returns the filtered features.
#
# Returns the selected Features according to their importance.
# Features with an importance value of 0 will be left out regardeless of the given \code{n}.
# 
# @param feat.importance [\code{numeric}]\cr 
#   Result of \code{\link{filterFeatures}}.
# @param n [\code{integer(1)}]\cr
#   Number of features ordered by the information value to select.
# @param threshold [\code{numeric(1)}]\cr
#   Information value as to be greater then the threshold. Default is 0.
# @return [\code{character}]
# getFilteredFeatures = function(feat.importance, n, threshold=0) {
  # checkArg(feat.importance, "numeric", nas.ok=FALSE) 
  # checkArg(threshold, "numeric", len=1L, na.ok=FALSE)
  # if (missing(n))
    # n = length(feat.importance)
  # checkArg(n, "integer", len=1L, lower=1L, na.ok=FALSE)
  # feats = feat.importance[feat.importance > threshold]
  # feats = head(feats[order(feats, decreasing=TRUE)], n)
  # results = names(feats)
  # #result = makeFeatSelResult(learner=NA, control=NA, x=names(feats), y=feats, opt.path=NA)
  # results
# }
