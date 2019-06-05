#' @title Normalize features.
#'
#' @description
#' Normalize features by different methods.
#' Internally [BBmisc::normalize] is used for every feature column.
#' Non numerical features will be left untouched and passed to the result.
#' For constant features most methods fail, special behaviour for this case is implemented.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param method (`character(1)`)\cr
#'   Normalizing method. Available are:\cr
#'   \dQuote{center}: Subtract mean.\cr
#'   \dQuote{scale}: Divide by standard deviation.\cr
#'   \dQuote{standardize}: Center and scale.\cr
#'   \dQuote{range}: Scale to a given range.\cr
#' @param cols ([character])\cr
#'   Columns to normalize. Default is to use all numeric columns.
#' @param range (`numeric(2)`)\cr
#'   Range for method \dQuote{range}.
#'   Default is `c(0,1)`.
#' @param on.constant (`character(1)`)\cr
#'   How should constant vectors be treated? Only used, of \dQuote{method != center},
#'   since this methods does not fail for constant vectors. Possible actions are:\cr
#'   \dQuote{quiet}: Depending on the method, treat them quietly:\cr
#'     \dQuote{scale}: No division by standard deviation is done, input values.
#'        will be returned untouched.\cr
#'     \dQuote{standardize}: Only the mean is subtracted, no division is done.\cr
#'     \dQuote{range}: All values are mapped to the mean of the given range.\cr
#'   \dQuote{warn}: Same behaviour as \dQuote{quiet}, but print a warning message.\cr
#'   \dQuote{stop}: Stop with an error.\cr
#' @template ret_taskdf
#' @seealso [BBmisc::normalize]
#' @export
#' @family eda_and_preprocess
normalizeFeatures = function(obj, target = character(0L), method = "standardize", cols = NULL,
  range = c(0, 1), on.constant = "quiet") {
  checkTargetPreproc(obj, target, cols)
  assertChoice(method, choices = c("range", "standardize", "center", "scale"))
  assertNumeric(range, len = 2L, any.missing = FALSE)

  UseMethod("normalizeFeatures")
}

#' @export
normalizeFeatures.data.frame = function(obj, target = character(0L), method = "standardize",
  cols = NULL, range = c(0, 1), on.constant = "quiet") {

  df = obj
  # get all numeric feature names present in data
  work.cols = colnames(df)[vlapply(df, is.numeric)]
  work.cols = setdiff(work.cols, target)

  # check that user requested cols are only numeric cols
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
  } else {
    cols = work.cols
  }

  df[, cols] = normalize(x = df[, cols, drop = FALSE], method = method,
    range = range, on.constant = on.constant)
  df
}

#' @export
normalizeFeatures.Task = function(obj, target = character(0L), method = "standardize",
  cols = NULL, range = c(0, 1), on.constant = "quiet") {
  target = getTaskTargetNames(obj)
  df = getTaskData(obj)
  normalized.df = normalizeFeatures(obj = df, target = target, method = method,
    cols = cols, range = range, on.constant = on.constant)
  changeData(obj, data = normalized.df)
}
