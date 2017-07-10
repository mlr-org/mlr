# @title Functional analysis task.
#
# @description
# Converts a normal task to a FDA task by adding some extra information
# (fd.features, fd.grids) to the task description and changing the S3
# classes to FDATask. \dQuote{task.cl} and \dQuote{desc.cl} are the names for
# the new class and task description respectively. It also does arg checking to
# ensure consistency.
#
# @param task [\code{link{Task}}]\cr
#   The normal mlr Task.
# @param type [\code{character}]\cr
#   The task type, will be set as a field to both task and task description. Either
#   \code{fdaregr} or \code{fdaclassif}.
# @template arg_fdatask_pars
# @param task.cl [\code{character}]\cr
#   The sub class name to convert to, eg. \code{FDAClassifTask} or \code{FDARegrTask}.
# @param desc.cl [\code{character}]\cr
#   The sub class description name to convert to, eg. \code{FDAClassifTaskDesc}
#   or \code{FDARegrTaskDesc}.
# @return An object of class \code{FDATask}.
# @aliases FDATask
convertTaskToFDATask = function(task, type, fd.features, fd.grids, task.cl, desc.cl) {
  fnames = getTaskFeatureNames(task)
  target = getTaskTargetNames(task)
  # type could be fdaregr or fdaclassif
  task$type = type
  task$task.desc$type = type
  assertList(fd.features, null.ok = TRUE, types = c("character", "integer"),
    any.missing = FALSE, min.len = 1L, names = "unique")
  assertList(fd.grids, null.ok = TRUE, types = "numeric",
    any.missing = FALSE, min.len = 1L, names = "unique")
  # if the user doesn't specify the fd.features, we assume all features belong to one group or one covariate
  if (is.null(fd.features))
    fd.features = list(fd1 = fnames)
  fdfns = names(fd.features)
  # if the user doesn't specify the fd.grids, we assume grids to be a list of 1,...,k
  if (is.null(fd.grids)) {
    fd.grids = lapply(fdfns, function(name) seq_along(fd.features[[name]]))
    names(fd.grids) = fdfns
  }
  # check if length of each fct.covariate is equivalent to the length of its grid
  for (g in fdfns) {
    n1 = length(fd.features[[g]])
    n2 = length(fd.grids[[g]])
    if (n1 != n2)
      stopf("For func. feat. '%s', length of fd.features entry (%i) does not match length of fd.grids entry (%i)!",
        g, n1, n2)
  }
  # we don't support mixed integer and character specification for fd.features
  assertLogical(unique(vlapply(fd.features, is.integer)), len = 1)
  assertNames(names(fd.grids), permutation.of = fdfns)
  # two functional covariate can not occupy the same variable
  assert(length(unlist(fd.features)) == length(unique(unlist(fd.features))))
  cns = colnames(getTaskData(task))
  # cns = c(getTaskTargetNames(task), getTaskFeatureNames(task)) can't be used, please do not use it!
  # lets check integrity of every entry of fd.features, then convert indices to character vector
  fd.features = lapply(fd.features, function(f) {
    if (is.character(f)) {
      assertSubset(f, fnames)
    } else if (is.integer(f)) {
      assertInteger(f, lower = 1L, upper = length(cns))
      f = cns[f]
    }
    return(f)
  })

  # ensure that fd.features and fd.features have same order
  fd.grids = fd.grids[fdfns]
  # check if target column is not used in fd.features
  if (target %in% unlist(fd.features))
    stopf("Target column cannot be included in 'fd.features'!")
  task$task.desc$fd.features = fd.features
  task$task.desc$fd.grids = fd.grids
  # if a variable does not belong to functional covariate, it is regarded as scalar
  task$task.desc$fd.scalars = getTaskFeatureNames(task)[getTaskFeatureNames(task) %nin% unlist(fd.features)]
  task$task.desc = addClasses(task$task.desc, desc.cl)
  addClasses(task, c(task.cl, "FDATask"))
}

#' @export
print.FDATask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  fdf = getTaskDesc(x)$fd.features
  # for every func covar get name and length
  s = vcapply(names(fdf), function(f) sprintf("%s (%i)", f, length(fdf[[f]])))
  catf("Functional features: %i\n%s", length(fdf), clipString(collapse(s, sep = ", "), 30L))
  catf("Scalar features: %i", getTaskNFeats(x) - sum(viapply(fdf, length)))
}

# Called in makeFDAClasifTask / makeFDARegrTask
# takes fd.features and fd.grids and updates them so only names from fd.remain are used
# eg when this happens: subset(FDATask, features = 1:10)
# feat.remain is a charvec of colnames, that we want to restrict our data to
# returns (list(fd.features, fd.grids))
updateFDAFeaturesAndGrids = function(fd.features, fd.grids, feat.remain) {
  # for each func feat: find out index of kept elements and also use this to restrict grid
  for (i in seq_along(fd.features)) {
    cns = fd.features[[i]]
    ok = cns %in% feat.remain
    fd.features[[i]] = cns[ok]
    fd.grids[[i]] = fd.grids[[i]][ok]
  }
  list(fd.features = fd.features, fd.grids = fd.grids)
}
