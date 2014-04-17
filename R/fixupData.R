# basic clean up of data. currently removes empty factor levels
fixupData = function(data, target, choice) {
  if (choice == "no")
    return(data)
  if (choice == "quiet")
    return(droplevels(data))
  # the next lines look a bit complicated, we calculate the warning info message
  cns = colnames(data)
  levs1 = lapply(data, function(x)
    if (is.factor(x)) levels(x) else NULL)
  data = droplevels(data)
  levs2 = lapply(data, function(x)
    if (is.factor(x)) levels(x) else NULL)
  j = sapply(cns, function(cn) any(levs1[[cn]] != levs2[[cn]]))
  if (any(j))
    warningf("Empty factor levels were dropped for columns: %s", collapse(cns[j]))
  return(data)
}
