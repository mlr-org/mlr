checkTarget = function(type, data, target, expected.len, supported.types) {
  checkArg(target, "character", len = expected.len, na.ok = FALSE)
  if (!all(target %in% colnames(data)))
    stopf("Column names of data don't contain target var: %s", collapse(target))

  # we might have 2 target cols for surv, iterate over them
  for (j in seq_along(target)) {
    tt = target[j]
    targetcol = data[, tt]
    targetclass = class(targetcol)
    st = supported.types[[j]]
    if (targetclass %nin% st)
      stopf("Target column %s has an unsupported type '%s' for task type '%s'.\nEither you made a mistake or you have to convert it.\n Supported types are: %s",
        tt, targetclass, type, collapse(st))
    # we check for NANs and INFs in whole data.frame in checkData
    if (any(is.na(targetcol)))
      stopf("Target column '%s' contains missing values!", tt)
  }
}
