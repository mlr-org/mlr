#' @title Generate dummy variables for factor features.
#'
#' @description
#' Replace all factor features with their dummy variables.
#'
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param intercept [\code{logical(1)}]\cr
#'   \code{TRUE} if for n levels you want n dummy variables.
#'   Otherwise \code{FALSE} gives you n-1 dummy variables.
#'   Default is \code{TRUE}.
#' @param exclude [\code{character}]\cr
#'   Names of the columns to exclude.
#'   The target does not have to be included here.
#'   Default is none.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as
#'   \code{obj}.
#' @seealso \code{\link{model.matrix}}
#' @export
createDummyFeatures = function(obj, target = NULL, intercept = TRUE, exclude = character(0L)) {
  checkArg(intercept, "logical", len = 1L, na.ok = FALSE)
  UseMethod("createDummyFeatures")
}

#' @export
createDummyFeatures.data.frame = function(obj, target = NULL, intercept = TRUE, exclude = character(0L)) {
  checkArg(exclude, subset = colnames(obj))
  # extract obj to work on
  work.cols = colnames(obj)[sapply(obj, is.factor)]
  if (isSet(exclude))
    work.cols = setdiff(work.cols, exclude)
  if (isSet(target))
    work.cols = setdiff(work.cols, target)
  dummies = lapply(work.cols, function(colname) {
    if (intercept) {
      res = model.matrix(~obj[[colname]]-1)
      colnames(res) = levels(obj[[colname]])
    } else {
      res = model.matrix(~obj[[colname]])
      colnames(res) = tail(levels(obj[[colname]]), -1)
    }
    res
  })
  #some effort to preserve order
  names(dummies) = work.cols
  col.list = convertColsToList(obj, factors.as.char = FALSE)
  for (col in work.cols) {
    col.list[[col]] = dummies[[col]]
  }
  do.call(cbind.data.frame, c(col.list, stringsAsFactors = FALSE))
}

#' @export
createDummyFeatures.SupervisedTask = function(obj, target = NULL, intercept = TRUE, exclude = character(0)) {
  checkArg(exclude, subset = getFeatureNames(obj))
  d = createDummyFeatures(obj = getTaskData(obj), target = obj$task.desc$target, intercept = intercept, exclude = exclude)
  changeData(obj, d)
}
