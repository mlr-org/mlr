#' Join some class existing levels to new, larger class levels for classification problems.
#'
#' @template arg_task
#' @param new.levels (`list` of `character`)\cr
#'   Element names specify the new class levels to create, while the corresponding element
#'   character vector specifies the existing class levels which will be joined to the new one.
#' @template ret_task
#' @export
#' @examples
#' joinClassLevels(iris.task, new.levels = list(foo = c("setosa", "virginica")))
joinClassLevels = function(task, new.levels) {
  UseMethod("joinClassLevels")
}

#' @export
joinClassLevels.ClassifTask = function(task, new.levels) {

  assertList(new.levels, types = "character", names = "unique")
  target = getTaskTargetNames(task)
  y = as.character(getTaskTargets(task))
  nls1 = unlist(new.levels)
  nls2 = unique(nls1)
  d = setdiff(nls2, unique(y))
  if (length(d) > 0L) {
    stopf("You can only recode already existing class levels, but you also used: %s", collapse(d))
  }
  if (length(nls2) != length(nls1)) {
    stopf("Every existing class level in 'new.levels' can be used at most once!")
  }
  new.names = names(new.levels)
  for (nn in new.names) {
    levs = new.levels[[nn]]
    y[y %in% levs] = nn
  }

  data = getTaskData(task)
  data[[target]] = as.factor(y)
  changeData(task, data)
}
