
#'
generateLearningCurve = function(lrn, task, test.size = 0.3, test.inds = NULL,
  n.seq = seq(0.1, 1, by = 0.1), measures, repls = 1L)  {

  lrn = checkLearner(lrn)
  assertClass(task, "Task")
  n = task$task.desc$size
  assertNumeric(n.seq, min.len = 2L, any.missing = FALSE)
  if (is.null(test.inds)) {
    test.inds = makeResampleInstance("Holdout", task = task, split = test.size)$train[[1L]]
  } else {
    test.inds = asInteger(test.inds)
  }
  measures = checkMeasures(measures, task)
  repls = asInt(repls, lower = 1L)
  k = length(n.seq)
  inds.all = setdiff(1:n, test.inds)
  perfs = array(NA, dim = c(repls, k, length(measures)))
  for (repl in 1:repls) {
    inds = sample(n.seq[1L])
    m.last = 0
    rest = inds.all
    inds.last = integer(0L)
    for (j in 1:k) {
      m = n.seq[j]
      more = (m - m.last) * n
      inds.new = sample(rest, more)
      inds.cur = c(inds.last, inds.new)
      mod = train(lrn, task, subset = inds.cur)
      pred = predict(mod, task, subset = test.inds)
      perfs[repl, j, ] = performance(pred, task = task, measures = measures)
      m.last = m
      inds.last = inds.cur
    }
  }
  dimnames(perfs) = list(1:repls, n.seq, sapply(measures, measureAggrName))
  aggr = apply(perfs, 2L, mean)
  return(list(perfs = perfs, aggr = aggr))
}

plotLearningCurve = function(res) {
  ggplot(aes_string(x = "n", y = y.name))
}


# r = generateLearningCurve("classif.rpart", iris.task, repls = 1)
plotLearningCurve(r)


