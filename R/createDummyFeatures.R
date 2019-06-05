#' @title Generate dummy variables for factor features.
#'
#' @description
#' Replace all factor features with their dummy variables. Internally [model.matrix] is used.
#' Non factor features will be left untouched and passed to the result.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param method (`character(1)`)\cr
#'   Available are:
#'   \describe{
#'     \item{"1-of-n":}{For n factor levels there will be n dummy variables.}
#'     \item{"reference":}{There will be n-1 dummy variables leaving out the first factor level of each variable.}
#'   }
#'   Default is \dQuote{1-of-n}.
#' @param cols ([character])\cr
#'   Columns to create dummy features for. Default is to use all columns.
#' @template ret_taskdf
#' @export
#' @family eda_and_preprocess
createDummyFeatures = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  assertChoice(method, choices = c("1-of-n", "reference"))
  if (!is.factor(obj) && !is.character(obj)) {
    checkTargetPreproc(obj, target, cols)
  }
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

  prefix = colnames(obj[work.cols])
  dfcol = obj[, work.cols]

  dummies = as.data.frame(lapply(obj[work.cols], createDummyFeatures, method = method))

  if (method == "reference" && length(work.cols) == length(dummies)) {
    colnames(dummies) = Map(function(col, pre) {
      stri_paste(pre, tail(levels(col), -1), sep = ".")
    }, obj[work.cols], prefix)
  }

  if (length(dummies) != 0) {
    if (ncol(dummies) == 1L) {
      colnames(dummies) = stri_paste(prefix, tail(levels(dfcol), -1), sep = ".")
    }
    cbind(dropNamed(obj, work.cols), dummies)
  } else {
    obj
  }
}

#' @export
createDummyFeatures.Task = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  target = getTaskTargetNames(obj)
  d = createDummyFeatures(obj = getTaskData(obj), target = target, method = method, cols = cols)
  changeData(obj, d)
}


#' @export
createDummyFeatures.factor = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  dcol = as.data.frame(obj)
  colname = colnames(dcol)
  if (method == "1-of-n") {
    form = stri_paste("~", colname, "-1")
    res = model.matrix(as.formula(form), data = dcol)
    colnames(res) = levels(as.factor(obj))
  } else {
    form = stri_paste("~", colname, "-1")
    res = model.matrix(as.formula(form), data = dcol)[, -1, drop = FALSE]
    colnames(res) = tail(levels(as.factor(obj)), -1)
  }
  as.data.frame(res)
}

#' @export
createDummyFeatures.character = function(obj, target = character(0L), method = "1-of-n", cols = NULL) {
  createDummyFeatures(as.factor(obj), method = method)
}
