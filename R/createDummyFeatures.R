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
#' @param cols [\code{character}]\cr
#'   Columns to create dummy features for. Default is to use all columns.
#' @template ret_taskdf
#' @export
#' @family eda_and_preprocess
createDummyFeatures = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  assertChoice(method, choices = c("1-of-n", "reference"))
  checkTargetPreproc(obj, target, cols)
  UseMethod("createDummyFeatures")
}

#' @export
createDummyFeatures.data.frame = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
    # get all factor feature names present in data
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, target)

  # check that user requested cols are only factor cols
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    work.cols = cols
  }

  # prevent function model.matrix from dropping rows with missing values
  old.na.action = options()$na.action
  on.exit(options(na.action = old.na.action))
  options(na.action = "na.pass")

  colname = colnames(obj[work.cols])
  dfcol = obj[,work.cols]

  dummies = lapply(obj[work.cols], createDummyFeatures.factor, method = method, colname = colname)
  #some effort to preserve order
  # names(dummies) = work.cols
  # col.list = convertColsToList(obj, factors.as.char = FALSE)
  # for (col in work.cols) {
  #   col.list[[col]] = dummies[[col]]
  # }
  # do.call(cbind.data.frame, c(col.list, stringsAsFactors = FALSE))
  cbind(dropNamed(obj,work.cols),dummies)
}

#' @export
createDummyFeatures.Task = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  target = getTaskTargetNames(obj)
  d = createDummyFeatures(obj = getTaskData(obj), target = target, method = method, cols = cols)
  changeData(obj, d)
}


#' @export
createDummyFeatures.factor = function (obj, method = "1-of-n", colname = "factor") {
  dcol = as.data.frame(obj)
  colnames(dcol) = colname
  if (method == "1-of-n") {
    form = stri_paste("~", colname, "-1")
    res = model.matrix(as.formula(form), data = dcol)
    colnames(res) = levels(obj)
  } else {
    form = stri_paste("~", colname, "-1")
    res = model.matrix(as.formula(form), data = dcol)[, -1, drop = FALSE]
    colnames(res) = tail(levels(obj), -1)
  }
  if (ncol(res) == 1) {
    colnames(res) = stri_paste(colname, colnames(res), sep = ".")
  }
  as.data.frame(res)
}

