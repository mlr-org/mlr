

generateLearningCurve = function(task, n.seq) {
  n = task$task.desc$size
  inds.all = 1:n
  inds = sample(n.seq[1L])
  for (m in n.seq) {
    more = m - m.last
    inds.new = sample(rest, more)
    ins.cur = c(inds.old, inds.new)
    mod = train(lrn, task, subset = inds.cur)
    pred = predict(mod, task, subset = "????")
    perf = performace(mod, task = task, measures = measures)
    m.last = m
    inds.last = inds.cur
  }
}
