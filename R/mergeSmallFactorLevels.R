#' @title Merges small levels of factors into new level.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param cols [\code{character}]
#'   Which columns to convert.
#'   Default is all factor and character columns.
#' @param min.perc [\code{numeric(1)}]\cr
#'   The smallest levels of a factor are merged until their combined proportion
#'   w.r.t. the length of the factor exceeds \code{min.perc}.
#'   Must be between 0 and 1.
#'   Default is 0.01.
#' @param new.level [\code{character(1)}]\cr
#'   New name of merged level.
#'   Default is \dQuote{.merged}
#' @return \code{factor}, where merged levels are combined into a new level of name \code{new.level}.
#' @family eda_and_preprocess
#' @export
mergeSmallFactorLevels = function(obj, target = NULL, cols = NULL, min.perc = 0.01, new.level = ".merged") {
  assertNumber(min.perc, lower = 0, upper = 1)
  assertString(new.level)
  UseMethod("mergeSmallFactorLevels")
}

#' @export
mergeSmallFactorLevels.Task = function(obj, target = NULL, cols = NULL, min.perc = 0.01, new.level = ".merged") {
  d = mergeSmallFactorLevels.data.frame(obj$env$data, obj$task.desc$target, cols, min.perc, new.level)
  changeData(obj, data = d)
}

#' @export
mergeSmallFactorLevels.data.frame = function(obj, target = NULL, cols = NULL, min.perc = 0.01, new.level = ".merged") {
  pred = function(x) is.factor(x) | is.character(x)
  cns = colnames(obj)[vlapply(obj, pred)]
  if (!is.null(cols)) {
    assertSubset(cols, cns)
    cns = intersect(cns, cols)
  }
  if (!is.null(target)) {
    assertSubset(target, colnames(obj))
    cns = setdiff(cns, target)
  }

  for (cn in cns) {
    x = as.factor(obj[[cn]])
    if (new.level %in% levels(x))
      stopf("Value of new.level = '%s' is already a level of column '%s'!", new.level, cn)
    j = which(prop.table(table(x)) < min.perc)
    if (length(j) > 0L) {
      levels(x)[levels(x) %in% names(j)] = new.level
      obj[[cn]] = x
    }
  }
  return(obj)
}
