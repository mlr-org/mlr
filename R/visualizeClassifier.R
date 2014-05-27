visualizeLearner = function(learner, task, features = NULL, ..., par.vals = list(),
  gridsize = 30, pointsize = 2) {

  checkLearner(learner, type = "classif")
  checkArg(task, "SupervisedTask")
  fns = getTaskFeatureNames(task)
  if (is.null(features))
    features = fns[1:2]
  else
    checkArg(features, subset = fns)
  par.vals = insert(list(...), par.vals)

  # subset to features, train learner with selected hyperpars
  task = subsetTask(task, features = features)
  lrn = setHyperPars(lrn, par.vals = par.vals)
  mod = train(lrn, task)

  # some shortcut names
  target = task$task.desc$target
  data = getTaskData(task)
  y = getTaskTargets(task)
  x1n = features[1L]
  x2n = features[2L]
  x1 = data[, x1n]
  x2 = data[, x2n]

  # setup data frames for ggplot. grid = background, data = points
  grid = expand.grid(
    seq(min(x1), max(x1), length.out = gridsize),
    seq(min(x2), max(x2), length.out = gridsize)
  )
  colnames(grid) = features
  p1 = predict(mod, newdata = grid)
  p2 = predict(mod, newdata = data)
  grid$.response = p1$data$response
  data$.response = p2$data$response
  data$.err = p2$data$response != y

  # plot stuff
  p = ggplot(grid, aes_string(x = x1n, y = x2n, z = ".response", fill = ".response"))
  p = p + geom_tile()
  # p = p + scale_fill_gradient2()
  # p = p + geom_contour(breaks = c(-1, 0, 1))
  p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, shape = target),
    size = pointsize)
  if (any(data$.err)) {
    p = p + geom_point(data = subset(data, data$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target),
      size = pointsize + .5, col = "orange")
  }
  title = sprintf("%s: %s", learner$id, paramValueToString(learner$par.set, par.vals))
  p = p + ggtitle(title)

  return(p)
}




