getTaskNFeats = function(task) {
  sum(task$task.desc$n.feat)
}

plotFilterValues = function(task, values, sort = "dec", nshow = 20L, feat.type.cols = c("red", "green")) {
  checkArg(task, "SupervisedTask")
  checkArg(values, "numeric")
  checkArg(sort, choices = c("dec", "inc", "none"))
  p = length(values)
  nshow = min(nshow, p)
  data = getTaskData(task)
  if (sort != "none")
    values = sort(values, decreasing = (sort == "dec"))
  ns = names(values)
  feat = factor(ns, levels = ns)
  d = data.frame(feat = feat, val = values)
  d = d[1:nshow,, drop = FALSE]
  if (!is.null(feat.type.cols)) {
    d$factor = sapply(as.character(d$feat), function(f)
      ifelse(is.factor(data[,f]), "Y", "N"))
    mp = aes_string(x = "feat", y = "val", fill = "factor")
  } else {
    mp = aes_string(x = "feat", y = "val")
  }
  p = ggplot(data = d, mapping = mp)
  p = p + geom_bar(stat = "identity")
  if (!is.null(feat.type.cols))
    p = p + scale_fill_manual(values = feat.type.cols)
  p = p + ggtitle(sprintf("Task '%s' with %i total features, filter = '%s'",
    task$task.desc$id, getTaskNFeats(task), "dddd"))
  p = p + xlab("") + ylab("")
  return(p)
}
