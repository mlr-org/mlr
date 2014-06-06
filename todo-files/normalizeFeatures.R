#' @title Normalize features by multiple methods
#'
#' @description
#' Normalize features by different methods in one step. For scaling \code{\link{scale}} is used.
#' 
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}} | \code{numeric}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param method [\code{character}]\cr
#'   Normalizing method. Multiple methods are possible. Ranging will be performed after scaling.\cr
#'   Available are:\cr
#'   \dQuote{center}: centering of each feature\cr 
#'   \dQuote{scale}: scaling of each feature\cr 
#'   \dQuote{standardize}: centering and scaling\cr
#'   \dQuote{range}: Scale the data to a given range.\cr 
#' @param range [\code{numeric(2)}]\cr
#'   Range the features should be scaled to. Default is \code{c(0,1)}.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}} | \code{numeric}]. Same type as \code{obj}.
#' @seealso \code{\link{scale}}
#' @export

normalizeFeatures = function(obj, target = NULL, methods = "standardize", columns = NULL, range = c(0,1)) {
  if(any(methods %nin% normalize.methods)) {
    missing.method = methods[methods %nin% normalize.methods]
    stopf("The method %s is not supported by normalizeFeatures!", paste(missing.method, collapse=","))
  }
  UseMethod("normalizeFeatures")
}

#' @export
normalizeFeatures.data.frame = function(obj, target = NULL, methods = "standardize", columns = NULL, range = c(0,1)) {
  if(isSet(target)) {
    checkArg(target, min.len=1, max.len=3, cl="character")
  }
  if(isSet(range)) {
    checkArg(range, cl="numeric", len=2, lower=0, upper=1)
  }
  
  # extract obj to work on
  numeric.cols = vlapply(obj, is.numeric)
  work.cols = names(numeric.cols)
  if(isSet(columns)) {
    work.cols = intersect(work.cols, columns)
  }
  if(isSet(target)) {
    work.cols = setdiff(work.cols, target)
  }
  work.obj = obj[,work.cols]
  
  # scale part
  work.obj = scaleFeatures(obj, methods)
  
  # range part
  if("range" %in% methods) {
    work.obj = apply(work.obj, 2, normalizeFeatures, methods = "range", range = range)
  }
  
  # bring back work.obj into obj
  obj[,work.cols] = work.obj
  obj
}

#' @export
normalizeFeatures.numeric = function(obj, target = NULL, methods = "standardize", columns = NULL, range = c(0,1)) {
  if(!all(methods == "range")){
    obj = scaleFeatures(obj, methods)
  }
  if("range" %in% methods) {
    rangex = range(obj)
    obj = (obj - rangex[1]) / (diff(rangex) / diff(range))
    obj = obj + range[1]
  }
  obj
}

#' @export
normalizeFeatures.SupervisedTask = function(obj, target = NULL, methods = "standardize", columns = NULL, range = c(0,1)) {
  d = normalizeFeatures(obj = getTaskData(obj), target = obj$task.desc$target, methods = methods, columns = columns, range = range)
  changeData(obj, d)
}

scaleFeatures = function(obj, methods) {
  do.scale = FALSE
  do.center = FALSE
  if(any(c("standardize", "scale") %in% methods)){
    do.scale = TRUE
  }
  if(any(c("standardize", "center") %in% methods)){
    do.center = TRUE
  }
  #FIXME: scale(x, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)) to obtain sd = 1 on uncentered data!
  if(any(unlist(scale.pars))) {
    obj = scale(obj, center = do.center, scale = do.scale)
  }
  obj
}

normalize.methods = c("standardize", "center", "range", "scale")
