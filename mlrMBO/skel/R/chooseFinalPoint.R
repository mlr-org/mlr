chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  df = as.data.frame(opt.path, discretes.as.factor=TRUE)
  input.names = setdiff(colnames(df), c(y.name, "dob", "eol"))
  switch(control$final.method,
    "last.proposed" = nrow(df),
    "best.true.y" = getOptPathBestIndex(opt.path, ties="random"),
    "best.predicted" = which(rank(ifelse(control$minimize, 1, -1) * 
      predict(model, newdata=df[, input.names, drop=FALSE])$data$response, ties.method="random") == 1L))
}
