# checks the column names of a data frame or matrix for validity
# currently we use this for 'data' and 'cost' object of a task
# we check that col names dont contain weird chars and are unique
checkColumnNames = function(data, name) {
  cns = colnames(data)
  lapply(cns, function(cn) {
    if (!deparse(as.name(cn), backtick = TRUE) == cn)
      stopf("Column name contains special characters: %s", cn)
  })
  dup = duplicated(cns)
  if (any(dup))
    stopf("Duplicated column names in '%s' are not allowed: %s", name, collapse(unique(cns[dup])))
}
