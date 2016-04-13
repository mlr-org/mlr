#' @title Generate dummy variables for factor features.
#'
#' @description
#' Replace all factor features with their dummy variables. Internally \code{\link{model.matrix}} is used.
#' Non factor features will be left untouched and passed to the result.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param method [\code{character(1)}]\cr
#'   Available are:\cr
#'   \dQuote{1-of-n}: For n factor levels there will be n dummy variables.\cr
#'   \dQuote{reference}: There will be n-1 dummy variables leaving out the first factor level of each variable.\cr
#' @template arg_exclude
#' @template ret_taskdf
#' @export
#' @family eda_and_preprocess
createDummyFeatures = function(obj, target = character(0L), method = "1-of-n", exclude = character(0L)) {
  assertChoice(method, choices = c("1-of-n", "reference"))
  checkTargetPreproc(obj, target, cols = NULL)
  assertCharacter(exclude)
  UseMethod("createDummyFeatures")
}

#' @export
createDummyFeatures.data.frame = function(obj, target = character(0L), method = "1-of-n", exclude = character(0L)) {
  assertSubset(exclude, choices = colnames(obj))
  # extract obj to work on
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, exclude)
  work.cols = setdiff(work.cols, target)
  # prevent function model.matrix from dropping rows with missing values
  old.na.action = options()$na.action
  on.exit(options(na.action = old.na.action))
  options(na.action = "na.pass")
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
createDummyFeatures.SupervisedTask = function(obj, target = character(0L), method = "1-of-n", exclude = character(0)) {
  assertSubset(exclude, choices = getTaskFeatureNames(obj))
  d = createDummyFeatures(obj = getTaskData(obj), target = obj$task.desc$target, method = method, exclude = exclude)
  changeData(obj, d)
}
