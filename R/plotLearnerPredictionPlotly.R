plotLearnerPredictionPlotly = function(learner, task, features = NULL, measures, cv = 10L,  ...,
                                 gridsize, pointsize = 2,
                                 prob.alpha = TRUE, se.band = TRUE,
                                 err.mark = "train",
                                 err.col = "white", err.size = pointsize,
                                 greyscale = FALSE, pretty.names = TRUE,
                                 alpha = 1, err.alpha = alpha) {
  
  require(plotly)
  learner = checkLearner(learner)
  assert(
    checkClass(task, "ClassifTask"),
    checkClass(task, "RegrTask"),
  )
  td = getTaskDescription(task)
  
  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features) && td$type != "classif") {
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else if (is.null(features) && td$type == "classif") {
    features = if (length(fns) == 1L) fns else fns[1:3]
  } else {
    assertCharacter(features, max.len = 3L)
    assertSubset(features, choices = fns)
  }
  taskdim = length(features)
  if (td$type == "classif" && taskdim != 3)
    stopf("Classification: currently only 3D plots supported in Plotly, not: %i", taskdim)
  if (td$type == "regr" && taskdim != 2)
    stopf("Regression: currently only 3D plots supported in Plotly, not: %i", taskdim)
  
  measures = checkMeasures(measures, task)
  cv = asCount(cv)
  
  if (missing(gridsize)) {
    gridsize = ifelse(taskdim == 1L, 500, 100)
  } else {
    gridsize = asCount(gridsize)
  }
  assertNumber(pointsize, lower = 0)
  assertFlag(prob.alpha)
  assertFlag(se.band)
  assertChoice(err.mark, choices = c("train", "cv", "none"))
  assertString(err.col)
  assertNumber(err.size, lower = 0)
  assertLogical(greyscale)
  
  if (td$type == "classif" && err.mark == "cv" && cv == 0L)
    stopf("Classification: CV must be switched on, with 'cv' > 0, for err.type = 'cv'!")
  
  # subset to features, set hyperpars
  task = subsetTask(task, features = features)
  learner = setHyperPars(learner, ...)
  
  # some shortcut names
  target = td$target
  data = getTaskData(task)
  y = getTaskTargets(task)
  x1n = features[1L]
  x1 = data[, x1n]
  
  # predictions
  # if learner supports prob or se, enable it
  if (td$type == "regr" && taskdim == 1L && hasLearnerProperties(learner, "se"))
    learner = setPredictType(learner, "se")
  if (td$type == "classif" && hasLearnerProperties(learner, "prob"))
    learner = setPredictType(learner, "prob")
  mod = train(learner, task)
  pred.train = predict(mod, task)
  yhat = pred.train$data$response
  perf.train = performance(pred.train, task = task, measures = measures)
  if (cv > 0L) {
    cv = crossval(learner, task, iters = 10L, measures = measures, show.info = FALSE)
    perf.cv = cv$aggr
    pred.cv = cv$pred
  } else {
    perf.cv = NA_real_
  }
  
  # 2d, 3d stuff
  if (taskdim > 1L) {
    x2n = features[2L]
    x2 = data[, x2n]
    if (taskdim == 3L) {
      x3n = features[3L]
      x3 = data[, x3n]
    }
  }
  
  # grid for predictions
  if (taskdim == 1L) {
    grid = data.frame(x = seq(min(x1), max(x1), length.out = gridsize))
  } else if (taskdim == 2L) {
    # setup data frames for ggplot. grid = background, data = points
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
  } else if (taskdim == 3L) {
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize / 4),
      seq(min(x2), max(x2), length.out = gridsize / 4),
      seq(min(x3), max(x3), length.out = gridsize / 4)
    )
  }
  colnames(grid) = features
  pred.grid = predict(mod, newdata = grid)
  grid[, target] = pred.grid$data$response
  
  # set title
  if (pretty.names) {
    lrn.str = learner$short.name
  } else {
    lrn.str = learner$id
  }
  title = sprintf("%s: %s", lrn.str, paramValueToString(learner$par.set, learner$par.vals))
  title = sprintf("%s\nTrain: %s; CV: %s", title, perfsToString(perf.train), perfsToString(perf.cv))
  
  if (td$type == "classif") {
    data$.err = if (err.mark == "train")
      (y != yhat)
    else if (err.mark == "cv")
      y != pred.cv$data[order(pred.cv$data$id), "response"]
    else
      NULL
    if (taskdim == 3L) {
      require(plotly)
      # print normal points
      if (greyscale)
        p = plot_ly(data = data, x = get(x1n), y = get(x2n), z = get(x3n), 
                    type = "scatter3d", mode = "markers", symbol = data[, target], 
                    marker = list(size = pointsize, opacity = alpha, color = "grey"), 
                    text = "Input Data", legendgroup = "Input Data")
      else 
        p = plot_ly(data = data, x = get(x1n), y = get(x2n), z = get(x3n), 
                    type = "scatter3d", mode = "markers", symbol = data[, target], 
                    marker = list(size = pointsize, opacity = alpha), 
                    text = "Input Data", legendgroup = "Input Data")
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n)),
                                    yaxis = list(title = paste("y: ", x2n)),
                                    zaxis = list(title = paste("z: ", x3n))))
      # print incorrect points
      if (err.col == "white")
        p = add_trace(p, data = subset(data, data$.err), 
                      x = get(x1n), y = get(x2n), z = get(x3n),
                      type = "scatter3d", mode = "markers", 
                      symbol = data[, target],
                      marker = list(size = err.size, opacity = err.alpha, color = "black"), 
                      text = "Missclassified Data", legendgroup = "Missclassified Data")
      else
        p = add_trace(p, data = subset(data, data$.err),
                      x = get(x1n), y = get(x2n), z = get(x3n),
                      type = "scatter3d", mode = "markers", 
                      symbol = data[, target],
                      marker = list(size = err.size, opacity = err.alpha, color = err.col), 
                      text = "Missclassified Data", legendgroup = "Missclassified Data")
    }
  } else if (td$type == "regr" && taskdim == 2L) {
    # reform grid data
    grid.dcast = reshape2::dcast(grid, as.formula(paste(x1n, x2n, sep = "~")), value.var = target)
    # generate 3D plots data list
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = t(as.matrix(grid.dcast[,-1])))
    if (greyscale) {
      # plot 3D surface
      p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, 
                  type = "surface", colorbar = list(title = target), name = "Learned Value", colorscale = "Greys")
      # set plot parameters
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                    zaxis = list(title = paste("z: ", target, sep = ""))))
      # add real value trace
      p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target], 
                    type = "scatter3d", color = data[, target], mode = "markers",
                    marker = list(size = pointsize, colorbar = F, colorscale = "Greys"), name = "Input Value", showscale = F)
    } else {
      # plot 3D surface
      p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, 
                  type = "surface", colorbar = list(title = target), name = "Learned Value")
      # set plot parameters
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                    zaxis = list(title = paste("z: ", target, sep = ""))))
      # add real value trace
      p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target], 
                    type = "scatter3d", color = data[, target], mode = "markers",
                    marker = list(size = pointsize, colorbar = F), name = "Input Value", showscale = F)
    }
  }
  return(p)
}