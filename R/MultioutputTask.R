#' @export
#' @rdname Task
makeMultioutputTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, coordinates = NULL, fixup.data = "warn", check.data = TRUE) {
  assertString(id)
  assertCharacter(target, any.missing = FALSE, min.len = 2L)
  assertDataFrame(data)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  ti = which(colnames(data) %in% target)
  tt = sapply(data[, ti], typeof)
  if (any(tt == "character")) stop("target data types cannot be character: please convert all characters to factors")

  if (all(tt == "logical")) {
    task = makeMultilabelTask(id = id, data = data, target = target, weights = weights, blocking = blocking,
      coordinates = coordinates, fixup.data = fixup.data, check.data = check.data)
  } else {
    if (all(tt == "double")) {
      task = makeMultiRegr(id = id, data = data, target = target, weights = weights, blocking = blocking,
        coordinates = coordinates, fixup.data = fixup.data, check.data = check.data)
    } else {
      task = makeSupervisedTask("mixedoutput", data = data, target = target,
        weights = weights, blocking = blocking,
        coordinates = coordinates, fixup.data = fixup.data,
        check.data = check.data)
      task$task.desc = makeMixedOutputTaskDesc(id, data, target, weights, blocking, coordinates)
      task = addClasses(task, "MixedOutputClass")
    }
  }
  return(task)
}

#' @export
print.MixedOutputClass = function(x, ...) {
  y = getTaskTargets(x)
  print.SupervisedTask(x)
  catf("Classes: %i", ncol(y))
  print(sapply(y, typeof))
}

#' @export
#' @rdname makeTaskDesc
makeMixedOutputTaskDesc = function(id, data, target, weights, blocking, coordinates) {
  levs = target
  td = makeTaskDescInternal("mixedoutput", id, data, target, weights, blocking, coordinates)
  td$class.levels = levs
  return(addClasses(td, c("MixedOutputTaskDesc", "SupervisedTaskDesc")))
}
