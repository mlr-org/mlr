# Checks the following things for a data.frame
# - accept integer, numeric and factor columns
# - No empty factor levels (can be auto-removed with fixupData)
# - No infinite values
# - No NANs
checkData = function(type, data, target) {
  mapply(function(x, cn) {
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
  }, x = data, cn = colnames(data))
}

