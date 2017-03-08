#' @title Functional analysis task.
#'
#' @description
#' Converts a normal task to a FDA task by adding some extra information
#' (fd.features, fd.grids) to #' the task description and changing the S3
#' classes to FDATask. \dQuote{task.cl} and \dQuote{desc.cl} are the names for
#' the new class and task description respectively. It also does arg checking to
#' ensure consistency.
#'
#' @param task [\code{link{Task}}]\cr
#'   The normal mlr Task.
#' @param type [\code{character}]\cr
#'   The task type, will be set as a field to both task and task description. Either
#'   \code{fdaregr} or \code{fdaclassif}.
#' @template arg_fdatask_pars
#' @param task.cl [\code{character}]\cr
#'   The sub class name to convert to, eg. \code{FDAClassifTask} or \code{FDARegrTask}.
#' @param desc.cl [\code{character}]\cr
#'   The sub class description name to convert to, eg. \code{FDAClassifTaskDesc}
#'   or \code{FDARegrTaskDesc}.
#' @return An object of class \code{FDATask}.
#' @export
#' @aliases FDATask
makeFDATask = function(task, type, fd.features, fd.grids, task.cl, desc.cl) {
  fnames = getTaskFeatureNames(task)
  # type could be fdaregr or fdaclassif
  task$type = type
  task$task.desc$type = type
  assertList(fd.features, null.ok = TRUE, types = c("character", "integer"),
    any.missing = FALSE, min.len = 1L, names = "unique")
  assertList(fd.grids, null.ok = TRUE, types = "numeric",
    any.missing = FALSE, min.len = 1L, names = "unique")
  # if the user doesn't specify the fd.features, we assume all features belong to one group or one covariate
  if (is.null(fd.features)) {
    fd.features = list(fd1 = fnames)
  }
  if (is.null(fd.grids)) {
    fd.grids = setNames(lapply(X = names(fd.features), FUN = function(name) {
        as.numeric(1:length(fd.features[[name]]))
      }), names(fd.features))
  }
  assertNames(names(fd.grids), permutation.of = names(fd.features))
  cns = colnames(getTaskData(task))
  # lets check integrity of every entry of fd.features, then convert indices to character vector
  fd.features = lapply(fd.features, function(f) {
    if (is.character(f)) {
      assert_subset(f, fnames)
    } else if (is.integer(f)) {
      assert_integer(f, lower = 1L, upper = length(cns))
      f = cns[f]
    }
    return(f)
  })
  task$task.desc$fd.features = fd.features
  task$task.desc$fd.grids = fd.grids
  task$task.desc = addClasses(task$task.desc, desc.cl)
  addClasses(task, c(task.cl, "FDATask"))
}

#' @export
print.FDATask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  fdf = getTaskDescription(x)$fd.features
  # for every func covar get name and length
  s = vcapply(names(fdf), function(f) sprintf("%s (%i)", f, length(fdf[[f]])))
  catf("Functional features: %i\n%s", length(fdf), clipString(collapse(s, sep = ", "), 30L))
  catf("Scalar features: %i", getTaskNFeats(x) - sum(viapply(fdf, length)))
}
