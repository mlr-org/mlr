#' @title Functional analysis task.
#'
#' @description
#' converts a "normal" task (like ClassifTask) to an FDA task (like a ClassifTaskDesc)
#' by adding some extra info to the taskdesc and changing s3 classes of task and desc
#' task.cl and desc.cl are the target classes for converted object
#' also does arg checking for FDA extra info args.

#' @export
#' @aliases FDATask
makeFDATask = function(task, type, fd.features, fd.grids, task.cl, desc.cl) {
  fnames = getTaskFeatureNames(task)
  task$type = type
  task$task.desc$type = type
  assertList(fd.features, null.ok = TRUE, types = c("character", "integer"),
    any.missing = FALSE, min.len = 1L, names = "unique")
  assertList(fd.grids, null.ok = TRUE, types = "numeric",
    any.missing = FALSE, min.len = 1L, names = "unique")
  if (is.null(fd.features)) {
    fd.features = list(fd1 = fnames)
  }
  if (is.null(fd.grids)) {
    for(name in names(fd.features)){
      fd.grids[[name]] = as.numeric(1:length(fd.features[[name]]))
    }
    #fd.grids = namedList(names(fd.features), init = as.numeric(1:(length(fnames)+1))) # this line is wrong way of initializing
  }
  assertNames(names(fd.grids), permutation.of = names(fd.features))
  cns = colnames(getTaskData(task))
  # lets check integrity of every entry of fd.features, then convert indices to character vector
  fd.features = lapply(fd.features, function(f) {
    if (is.character(f)) {
      rest = setdiff(f, fnames)
      if (length(rest) > 0L)
        stopf("Functional features must use only data column names, not: '%s'",
          clipString(collapse(rest), 30L))
      return(f)
    }
    if (is.integer(f)) {
      if (!all(f >= 1L & f <= length(cns)))
        stop("Functional features indices must be between 1 and ncols(data)!")
      return(cns[f])
    }
  })
  task$task.desc$fd.features = fd.features
  task$task.desc$fd.grids = fd.grids
  task$task.desc = addClasses(task$task.desc, desc.cl)
  addClasses(task, c(task.cl, "FDATask"))
}

#' @export
print.FDATask = function(x, ...) {
  print.SupervisedTask(x, print.target = FALSE, print.weights = FALSE)
  fdf = x$task.desc$fd.features
  # for every func covar get name and length
  s = vcapply(names(fdf), function(f) sprintf("%s (%i)", f, length(fdf[[f]])))
  catf("Functional features: %i\n%s", length(fdf), clipString(collapse(s, sep = ", "), 30L))
}

