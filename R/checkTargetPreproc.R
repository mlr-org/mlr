# checking consistency of target name in data and cols
# we use this in several preprocessing functions like impute
checkTargetPreproc = function(data, target, cols) {
  if (length(target != 0L)) {
    not.ok = which.first(target %nin% names(data))
    if (length(not.ok) != 0L)
      stopf("Target column '%s' must be present in data", target[not.ok])
    not.ok = which.first(target %in% names(cols))
    if (length(not.ok) != 0L)
      stopf("Preprocessing of target column '%s' not possible", target[not.ok])
  }
}
