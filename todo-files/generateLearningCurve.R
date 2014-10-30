
#'
generateLearningCurve = function(lrn, task, test.size = 0.3, test.inds = NULL,
  n.seq = seq(0.1, 1, by = 0.1), measures, repls = 1L)  {

  assertClass(task, "Task")
  assertNumeric(test.size, lower = 0L, upper = 1L, len = 1)
  assertNumeric(n.seq, lower = 0L, upper = 1L, min.len = 2L, any.missing = FALSE)
  repls = asInt(repls, lower = 1L)
  
  if (is.null(test.inds)) {
    test.inds = makeResampleInstance("Holdout", task = task, split = test.size)$train[[1L]]
  } else {
    test.inds = asInteger(test.inds)
  }
  
  l = length(lrn)
  for (i in 1:l) {
    lrn[[i]] = checkLearner(lrn[[i]])
  }
  
  m = length(measures)
  measures = checkMeasures(measures, task)
  
  n = task$task.desc$size
  k = length(n.seq)
  inds.all = setdiff(1:n, test.inds)
  
  perfs = replicate(l, array(NA, dim = c(repls, k, m)), simplify = FALSE)
  lrn.names = unlist(lapply(lrn, function(x) x$short.name))
  measure.names = sapply(measures, measureAggrName)
  names(perfs) = lrn.names
  for (i in 1:l) {
    dimnames(perfs[[i]]) = list(1:repls, n.seq, measure.names)
  }
  
  more = diff(c(0, round(n.seq * (n - length(test.inds)))))
  n.obs = cumsum(more)
  for (repl in 1:repls) {
    rest = inds.all
    inds.last = integer(0L)
    for (j in 1:k) {
      inds.new = sample(rest, more[j])
      inds.cur = c(inds.last, inds.new)
      for (i in 1:l) {
        mod = train(lrn[[i]], task, subset = inds.cur)
        pred = predict(mod, task, subset = test.inds)
        perfs[[i]][repl, j, ] = performance(pred, task = task, measures = measures)
      }
      inds.last = inds.cur
    }
  }
  
  res = data.frame()
  for (j in 1:l) {
    for (i in 1:m) {
      new = data.frame(n.obs = n.obs, perfs = colMeans(perfs[[j]])[,i],
                       measure = measure.names[i], learner = lrn.names[j])
      res = rbind(res, new)
    }
  }
  rownames(res) = NULL
  
  return(res)
}

plotLearningCurve = function(res) {
  library(ggplot2)
  ggplot(res, aes(x = n.obs, y = perfs, colour = learner)) + layer(geom = "point") +
    layer(geom = "line") + facet_wrap(~measure)
}


# r1 = generateLearningCurve(lrn = list("classif.rpart", "classif.knn", "classif.svm"),
#                           task = iris.task, measures = list(mmce, acc), repls = 12L)
# plotLearningCurve(r1)

# r2 = generateLearningCurve(lrn = list("classif.rpart", "classif.knn", "classif.naiveBayes",
#                                       "classif.svm", "classif.plr", "classif.randomForest"),
#                            task = sonar.task, test.size = 0.25, n.seq = seq(0.2, 1, by = 0.2),
#                            measures = list(tp, fp, tn, fn), repls = 6L)
# plotLearningCurve(r2)

