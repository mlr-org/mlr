#' @title Merges small levels of factors into new level.
#'
#' @description
#' Merges factor levels that occur only infrequently into combined levels with a higher frequency.
#'
#' @template arg_task
#' @param cols ([character])
#'   Which columns to convert.
#'   Default is all factor and character columns.
#' @param min.perc (`numeric(1)`)\cr
#'   The smallest levels of a factor are merged until their combined proportion
#'   w.r.t. the length of the factor exceeds `min.perc`.
#'   Must be between 0 and 1.
#'   Default is 0.01.
#' @param new.level (`character(1)`)\cr
#'   New name of merged level.
#'   Default is \dQuote{.merged}
#' @return `Task`, where merged levels are combined into a new level of name `new.level`.
#' @family eda_and_preprocess
#' @export
mergeSmallFactorLevels = function(task, cols = NULL, min.perc = 0.01, new.level = ".merged") {

  assertClass(task, "Task")
  assertNumber(min.perc, lower = 0, upper = 1)
  assertString(new.level)

  data = getTaskData(task)
  pred = function(x) is.factor(x) | is.character(x)
  cns = colnames(data)[vlapply(data, pred)]
  if (!is.null(cols)) {
    assertSubset(cols, cns)
    cns = intersect(cns, cols)
  }
  cns = setdiff(cns, getTaskTargetNames(task))

  for (cn in cns) {
    x = as.factor(data[[cn]])
    if (new.level %in% levels(x)) {
      stopf("Value of new.level = '%s' is already a level of column '%s'!", new.level, cn)
    }
    j = which(prop.table(table(x)) < min.perc)
    if (length(j) > 0L) {
      levels(x)[levels(x) %in% names(j)] = new.level
      data[[cn]] = x
    }
  }
  changeData(task, data = data)
}
