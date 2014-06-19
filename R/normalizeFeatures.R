#' @title Normalize features
#'
#' @description
#' Normalize features by different methods. Internally \code{\link{normalize}} is used.
#'
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}} | \code{numeric}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param method [\code{character(1)}]\cr
#'   Normalizing method.\cr
#'   Available are:\cr
#'   \dQuote{center}: centering of each feature\cr
#'   \dQuote{scale}: scaling of each feature\cr
#'   \dQuote{standardize}: centering and scaling\cr
#'   \dQuote{range}: Scale the data to a given range.\cr
#' @param exclude [\code{character}]\cr
#'   Names of the columns to exclude.
#' @param range [\code{numeric(2)}]\cr
#'   Range the features should be scaled to. Default is \code{c(0,1)}.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}} | \code{numeric}]. Same type as \code{obj}.
#' @seealso \code{\link{normalize}}
#' @export

normalizeFeatures = function(obj, target = NULL, method = "standardize", exclude = NULL, range = c(0,1)) {
  checkArg(method, choices = c("range", "standardize", "center", "scale"))
  UseMethod("normalizeFeatures")
}

#' @export
normalizeFeatures.data.frame = function(obj, target = NULL, method = "standardize", exclude = NULL, range = c(0,1)) {
  # extract obj to work on
  work.cols = colnames(obj)[sapply(obj, is.numeric)]
  if (isSet(exclude))
    work.cols = setdiff(work.cols, exclude)
  if (isSet(target))
    work.cols = setdiff(work.cols, target)
  work.obj = obj[,work.cols]

  work.obj = normalize(x = obj, method = method, range = range)

  # bring back work.obj into obj
  obj[,work.cols] = work.obj
  obj
}

#' @export
normalizeFeatures.SupervisedTask = function(obj, target = NULL, method = "standardize", exclude = NULL, range = c(0,1)) {
  d = normalizeFeatures(obj = getTaskData(obj), target = obj$task.desc$target, method = method, exclude = exclude, range = range)
  changeData(obj, d)
}
