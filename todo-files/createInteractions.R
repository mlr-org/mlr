createInteractions = function(obj, target = character(0L), cols = character(0L), types = "factor", exclude = character(0L), degree = 2) {
  UseMethod("createInteractions")
}

createInteractions.data.frame = function(obj, target = character(0L), cols = character(0L), types = "factor", exclude = character(0L), degree = 2) {
  assertCharacter(target, len = 1)
  assertCharacter(cols)
  assertCharacter(types)
  assertInt(degree, lower = 1)
  assertSubset(target, colnames(obj))
  assertSubset(cols, colnames(obj))
  if (degree = 1)
    return(obj)
  
  type.cols = colnames(obj)[vlapply(obj, function(x) any(class(x) %in% types))]
  work.cols = union(cols, type.cols)
  work.cols = setdiff(work.cols, c(target, exclude))
  
  form = as.formula(paste0("~ (", paste(work.cols,collapse = "+"), ")^", degree, "-1"))
  new.data = model.matrix(form, obj[, work.cols])
  new.data = new.data[,attr(new.data, "assign") > length(work.cols)] #take only the interactions
  cbind(obj, new.data)
}