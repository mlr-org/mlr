# Checks the following things for a data.frame
# - error-proof column names
# - No missing values in target
# - accept integer, numeric and factor columns
# - No empty factor levels
# - No infinite values
# - No NANs
checkData = function(data, target) {
  # FIXME: one should probably be able to disable some of these checks via configureMLR
  mapply(function(x, cn, is.target) {
    if (is.target && any(is.na(x)))
      stop("Target contains missing values!")
    if (!deparse(as.name(cn), backtick=TRUE) == cn)
      stopf("Column name contains special characters: %s", cn)
    if (is.numeric(x)) {
      if (any(is.infinite(x)))
        stopf("Data contains infinite values in: %s", cn)
      if (any(is.nan(x)))
        stopf("Data contains NaN values in: %s", cn)
    } else if (is.factor(x)) {
      if(any(table(x) == 0L))
        stopf("Data contains empty factor levels in: %s", cn)
    } else {
      stopf("Unsupported feature type in: %s, %s", cn, class(x)[1L])
    }
  }, x = data, cn = colnames(data), is.target = colnames(data) %in% target)
}

checkColumnNames = function(data, target) {
  cns = colnames(data)
  dup = duplicated(cns)
  if (any(dup))
    stopf("Duplicated column names in data are not allowed: %s", collapse(unique(cns[dup])))
  if (!all(target %in% cns)) {
    stopf("Column names of data don't contain target var: %s", collapse(target))
  }
}
