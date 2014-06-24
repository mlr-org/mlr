#' @title Generate dummy variables for factor features.
#'
#' @description
#' Replace all factor features with their dummy variables. Internally \code{\link{model.matrix}} is used.
#'
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param method [\code{logical(1)}]\cr
#'   Available are:\cr
#'   \dQuote{1-of-n}: For n factor levels there will be n dummy variables.\cr
#'   \dQuote{reference}: There will be n-w dummy variables leaving out the first factor level of each variable.\cr
#' @param exclude [\code{character}]\cr
#'   Names of the columns to exclude.
#'   The target does not have to be included here.
#'   Default is none.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as
#'   \code{obj}.
#' @seealso \code{\link{model.matrix}}
#' @export
createDummyFeatures = function(obj, target = NULL, method = "1-of-n", exclude = character(0L)) {
  checkArg(method, choices = c("1-of-n", "reference"))
  UseMethod("createDummyFeatures")
}

#' @export
createDummyFeatures.data.frame = function(obj, target = NULL, method = "1-of-n", exclude = character(0L)) {
  checkArg(exclude, subset = colnames(obj))
  # extract obj to work on
  work.cols = colnames(obj)[sapply(obj, is.factor)]
  if (isSet(exclude))
    work.cols = setdiff(work.cols, exclude)
  if (isSet(target))
    work.cols = setdiff(work.cols, target)
  dummies = lapply(work.cols, function(colname) {
    if (method == "1-of-n") {
      form = paste0("~",colname,"-1")
      res = model.matrix(as.formula(form), data = obj)
      colnames(res) = levels(obj[[colname]])
    } else {
      form = paste0("~",colname,"-1")
      res = model.matrix(as.formula(form), data = obj)[, -1, drop = FALSE]
      colnames(res) = tail(levels(obj[[colname]]), -1)
    }
    if (ncol(res) == 1) {
      colnames(res) = paste(colname, colnames(res), sep = ".")
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
createDummyFeatures.SupervisedTask = function(obj, target = NULL, method = "1-of-n", exclude = character(0)) {
  checkArg(exclude, subset = getFeatureNames(obj))
  d = createDummyFeatures(obj = getTaskData(obj), target = obj$task.desc$target, method = method, exclude = exclude)
  changeData(obj, d)
}
