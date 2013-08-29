makeTaskDescFromChangedData = function(task, data) {
  td = task$task.desc
  i = which(colnames(data) %in% c(target))
  td$size = nrow(data)
  y = data[, td$target]
  td$n.feat = c(
    numerics = sum(sapply(data, is.numeric)) - is.numeric(y), 
    factors = sum(sapply(data, is.factor)) - is.factor(y)
  )
  if(type == "classif")
    td$class.levels = levels(y)
  else
    td$class.levels = as.character(NA)
  td$has.missings = any(sapply(data, function(x) any(is.na(x))))
  return(td)
}