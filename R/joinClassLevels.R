#' Join some class existing levels to new, larger class levels for classification problems.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param new.levels [\code{list} of \code{character}]\cr
#'   Element names specify the new class levels to create, while the corresponding element
#'   character vector specifies the existing class levels which will be joined to the new one.
#' @template ret_taskdf
#' @export
#' @examples
#' joinClassLevels(iris.task, new.levels = list(foo = c("setosa", "virginica")))
joinClassLevels = function(obj, target, new.levels) {
  UseMethod("joinClassLevels")
}

joinClassLevels.ClassifTask = function(obj, target, new.levels) {
  d = joinClassLevels(obj = getTaskData(obj), target = obj$task.desc$target, new.levels = new.levels)
  changeData(obj, d)
}

joinClassLevels.data.frame = function(obj, target, new.levels) {
  assertClass(obj, "data.frame")
  cns = colnames(obj)
  assertSubset(target, cns)
  y = obj[, target]
  if (!(is.factor(y) || is.character(y)))
    stopf("Target column '%s' must be a factor or character vector, not a: %s", target, class(y)[1L])
  assertList(new.levels, type = "character", names = "unique")
  y = as.character(y)
  nls1 = unlist(new.levels)
  nls2 = unique(nls1)
  d = setdiff(nls2, unique(y))
  if (length(d) > 0L)
    stopf("You can only recode already existing class levels, but you also used: %s", collapse(d))
  if (length(nls2) != length(nls1))
    stopf("Every existing class level in 'new.levels' can be used at most once!")
  new.names = names(new.levels)
  for (nn in new.names) {
    levs = new.levels[[nn]]
    y[y %in% levs] = nn
  }
  obj[, target] = as.factor(y)
  return(obj)
}


