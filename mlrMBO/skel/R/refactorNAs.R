# for numeric columns:
#   NAs are replaced with the min(col)/max(col) * impute.multiplicator.
#   if min or max is used depends on the sign of impute.multiplicator
# for factors columns:
#   New level "na.string" is added and NAs replaced with this string value
refactorNAs <- function(data, impute.multiplicator=2, na.string="NA") {
  as.data.frame(lapply(data, function(x) {
    if (is.numeric(x)) {
      i = is.na(x)
      impute.val = range(x, na.rm=TRUE)[(impute.multiplicator >= 0) + 1L] * impute.multiplicator
      return(replace(x, i, impute.val))
    }
    if (is.factor(x)) {
      i = is.na(x)
      if (any(i)) {
        levels(x) = c(levels(x), na.string)
        return(replace(x, i, na.string))
      }
      return(x)
    }
    stopf("Type unsupported: %s", typeof(x))
  }), stringsAsFactors=FALSE)
}
