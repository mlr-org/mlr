
#'
generateLearningCurve = function(lrn, task, test.size = 0.3, test.inds = NULL,
  n.seq = seq(0.1, 1, by = 0.1), measures, repls = 1L)  {

  l = length(lrn)
  for (i in 1:l) {
    lrn[[i]] = checkLearner(lrn[[i]])
  }
  
  # assertClass(task, "Task")
  n = task$task.desc$size
  # assertNumeric(n.seq, min.len = 2L, any.missing = FALSE)
  if (is.null(test.inds)) {
    test.inds = makeResampleInstance("Holdout", task = task, split = test.size)$train[[1L]]
  } else {
    test.inds = asInteger(test.inds)
  }
  measures = checkMeasures(measures, task)
  # repls = asInt(repls, lower = 1L)
  k = length(n.seq)
  inds.all = setdiff(1:n, test.inds)
  
  perfs = replicate(l, array(NA, dim = c(repls, k, length(measures))),
                    simplify = FALSE)
  lrn.names = unlist(lapply(lrn, function(x) x$short.name))
  measure.names = sapply(measures, measureAggrName)
  names(perfs) = lrn.names
  for (i in 1:l) {
    dimnames(perfs[[i]]) = list(1:repls, n.seq, measure.names)
  }
  
  n.obs = numeric(length(n.seq))
  for (repl in 1:repls) {
    # inds = sample(n.seq[1L])
    m.last = 0
    rest = inds.all
    inds.last = integer(0L)
    for (j in 1:k) {
      m = n.seq[j]
      # FIXME: Shouldn't it be
      #        more = (m - m.last) * (n - length(test.inds))?
      more = (m - m.last) * n
      inds.new = sample(rest, more)
      inds.cur = c(inds.last, inds.new)
      if (repl == 1) {
        n.obs[j] = length(inds.cur) 
      }
      for (i in 1:l) {
        mod = train(lrn[[i]], task, subset = inds.cur)
        pred = predict(mod, task, subset = test.inds)
        perfs[[i]][repl, j, ] = performance(pred, task = task, measures = measures)
      }
      m.last = m
      inds.last = inds.cur
    }
  }
  
  res = data.frame()
  for (j in 1:l) {
    for (i in 1:length(measures)) {
      new = data.frame(n.obs = n.obs, perfs = colMeans(perfs[[j]])[,i],
                       measure = measure.names[i],
                       learner = lrn.names[j])
      res = rbind(res, new)
    }
  }
  
  return(res)
}

plotLearningCurve = function(res) {
  library(ggplot2)
  ggplot(res, aes(x = n.obs, y = perfs, colour = learner)) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure)
}


# r = generateLearningCurve(lrn = list("classif.rpart", "classif.lda", "classif.knn"),
#                           task = iris.task, measures = list(mmce, acc), repls = 12L)
# plotLearningCurve(r)
