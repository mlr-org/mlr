# for numeric columns:
#   NAs are replaced with the 2 * upper bound
# for factor columns:
#   NAs are coded as a new level
refactorNAs = function(data, par.set, na.string="miss") {
  cnd = colnames(data)
  upp = 2 * getUpper(par.set)
  pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  #FIXME bounds wont work with vec params currently, at least check this, see with.nr=FALSE?
  as.data.frame(sapply(colnames(data), simplify=FALSE, function(pid) {
    x = data[[pid]]
    # do not handle numeric y column, only inputs
    if (is.numeric(x) && pid %in% pids) {
      inds = is.na(x)
      return(replace(x, inds, upp[[pid]]))
    } else if (is.factor(x)) {
      inds = is.na(x)
      if (any(inds)) {
        levels(x) = c(levels(x), na.string)
        return(replace(x, inds, na.string))
      }
    }
    return(x)
  }), stringsAsFactors=FALSE)
}
