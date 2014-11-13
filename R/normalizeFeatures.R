#' @title Normalize features
#'
#' @description
#' Normalize features by different methods. Internally \code{\link{normalize}} is used.
#' Non numerical features will be left untouched and passed to the result.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   Normalizing method.\cr
#'   Available are:\cr
#'   \dQuote{center}: centering of each feature\cr
#'   \dQuote{scale}: scaling of each feature\cr
#'   \dQuote{standardize}: centering and scaling\cr
#'   \dQuote{range}: Scale the data to a given range.\cr
#' @template arg_exclude
#' @param range [\code{numeric(2)}]\cr
#'   Range the features should be scaled to. Default is \code{c(0,1)}.
#' @template ret_task
#' @export
#' @family eda_and_preprocess
normalizeFeatures = function(task, method = "standardize", exclude = character(0L), range = c(0, 1)) {
  assertClass(task, "Task")
  data = getTaskData(task)
  target = getTaskTargetNames(task)
  assertChoice(method, choices = c("range", "standardize", "center", "scale"))
  assertCharacter(exclude)
  assertNumeric(range, len = 2L, any.missing = FALSE)

  cols = colnames(data)[vlapply(data, is.numeric)]
  cols = setdiff(cols, c(exclude, target))
  data[, cols] = normalize(x = data[, cols, drop = FALSE], method = method, range = range)
  changeData(task, data = data)
}
