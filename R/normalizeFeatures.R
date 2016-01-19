#' @title Normalize features.
#'
#' @description
#' Normalize features by different methods. 
#' Internally \code{\link[BBmisc]{normalize}} is used for every feature column.
#' Non numerical features will be left untouched and passed to the result.
#' For constant features most methods fail, special behaviour for this case is implemented.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Normalizing method. Available are:\cr
#'   \dQuote{center}: Subtract mean.\cr
#'   \dQuote{scale}: Divide by standard deviation.\cr
#'   \dQuote{standardize}: Center and scale.\cr
#'   \dQuote{range}: Scale to a given range.\cr
#' @template arg_exclude
#' @param range [\code{numeric(2)}]\cr
#'   Range for method \dQuote{range}.
#'   Default is \code{c(0,1)}.
#' @param on.constant [\code{character(1)}]\cr
#'   How should constant vectors be treated? Only used, of \dQuote{method != center},
#'   since this methods does not fail for constant vectors. Possible actions are:\cr
#'   \dQuote{quiet}: Depending on the method, treat them quietly:\cr
#'     \dQuote{scale}: No division by standard deviation is done, input values.
#'        will be returned untouched.\cr
#'     \dQuote{standardize}: Only the mean is subtracted, no division is done.\cr
#'     \dQuote{range}: All values are mapped to the mean of the given range.\cr
#'   \dQuote{warn}: Same behaviour as \dQuote{quiet}, but print a warning message.\cr
#'   \dQuote{stop}: Stop with an error.\cr
#' @template ret_task
#' @seealso \code{\link[BBmisc]{normalize}}
#' @export
#' @family eda_and_preprocess
normalizeFeatures = function(task, method = "standardize", exclude = character(0L), range = c(0, 1), on.constant = "quiet") {
  assertClass(task, "Task")
  data = getTaskData(task)
  target = getTaskTargetNames(task)
  assertChoice(method, choices = c("range", "standardize", "center", "scale"))
  assertCharacter(exclude)
  assertNumeric(range, len = 2L, any.missing = FALSE)

  cols = colnames(data)[vlapply(data, is.numeric)]
  cols = setdiff(cols, c(exclude, target))
  data[, cols] = normalize(x = data[, cols, drop = FALSE], method = method, range = range, on.constant = on.constant)
  changeData(task, data = data)
}
