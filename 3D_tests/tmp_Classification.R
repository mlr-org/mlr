classif.lrn = makeLearner("classif.ksvm")
measures = list(getDefaultMeasure(obj))

learner = classif.lrn
task = iris.task
features = c("Sepal.Length", "Sepal.Width", "Petal.Length")
measures
cv = 10L
gridsize
pointsize = 2
prob.alpha = TRUE
se.band = TRUE
err.mark = "train"
bg.cols = c("darkblue", "green", "darkred")
err.col = "white"
err.size = pointsize
greyscale = FALSE
pretty.names = TRUE

plotLearnerPrediction = function(learner, task, features = NULL, three.d = FALSE, measures, cv = 10L,  ...,
                                 gridsize, pointsize = 2,
                                 prob.alpha = TRUE, se.band = TRUE,
                                 err.mark = "train",
                                 bg.cols = c("darkblue", "green", "darkred"),
                                 err.col = "white", err.size = pointsize,
                                 greyscale = FALSE, pretty.names = TRUE) {
  
  learner = checkLearner(learner)
  assert(
    checkClass(task, "ClassifTask"),
    checkClass(task, "RegrTask"),
    checkClass(task, "ClusterTask")
  )
  td = getTaskDescription(task)
  
  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features) && !three.d) {
    # take first or first 2 features as default
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else if (three.d && td$type != "classif") {
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else if (three.d && td$type == "classif") {
    features = if (length(fns) == 1L) fns else fns[1:3]
  } else {
    assertCharacter(features, max.len = 3L)
    assertSubset(features, choices = fns)
  }
  taskdim = length(features)
  if (td$type %in% c("classif", "cluster") && taskdim %nin% 2:3)
    stopf("Classification and clustering: currently only 2D and 3D plots supported, not: %i", taskdim)
  if (td$type == "regr" && taskdim %nin% 1:2)
    stopf("Regression: currently only 1D, 2D and 3D plots supported, not: %i", taskdim)
  
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
  if (td$type != "cluster")
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
    if (!three.d){
      if (taskdim == 2L) {
        p = ggplot(grid, aes_string(x = x1n, y = x2n))
        if (hasLearnerProperties(learner, "prob") && prob.alpha) {
          # max of rows is prob for selected class
          prob = apply(getPredictionProbabilities(pred.grid, cl = td$class.levels), 1, max)
          grid$.prob.pred.class = prob
          p = p + geom_tile(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
                            show.legend = TRUE)
          p = p + scale_alpha(limits = range(grid$.prob.pred.class))
        } else {
          p = p + geom_tile(mapping = aes_string(fill = target))
        }
        # print normal points
        p = p + geom_point(data = subset(data, !data$.err),
                           mapping = aes_string(x = x1n, y = x2n, shape = target), size = pointsize)
        # mark incorrect points
        if (err.mark != "none" && any(data$.err)) {
          p = p + geom_point(data = subset(data, data$.err),
                             mapping = aes_string(x = x1n, y = x2n, shape = target),
                             size = err.size + 1.5, show.legend = FALSE)
          p = p + geom_point(data = subset(data, data$.err),
                             mapping = aes_string(x = x1n, y = x2n, shape = target),
                             size = err.size + 1, col = err.col, show.legend = FALSE)
        }
        # print error points
        p = p + geom_point(data = subset(data, data$.err),
                           mapping = aes_string(x = x1n, y = x2n, shape = target), size = err.size, show.legend = FALSE)
        p  = p + guides(alpha = FALSE)
      }
    } else if (taskdim == 3L) {
      require(plotly)
      subdata = subset(data, !data$.err)
      errdata = subset(data, data$.err)
      p = plot_ly(x = subdata[, x1n], y = subdata[, x2n], z = subdata[, x3n], 
                  type = "scatter3d", mode = "markers", symbol = subdata[, target], 
                  marker = list(size = pointsize), name = "Input Value")
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n)),
                                    yaxis = list(title = paste("y: ", x2n)),
                                    zaxis = list(title = paste("z: ", x3n))))
      p = add_trace(p, x = errdata[, x1n], y = errdata[, x2n], z = errdata[, x3n],
                    type = "scatter3d", mode = "markers", symbol = errdata[, target],
                    marker = list(size = pointsize), name = "Missclassified")
    }
  } else if (td$type == "cluster") {
    if (taskdim == 2L) {
      data$response = factor(yhat)
      p = ggplot(data, aes_string(x = x1n, y = x2n, col = "response"))
      p = p + geom_point(size = pointsize)
    }
  } else if (td$type == "regr"  && !three.d) {
    if (taskdim == 1L) {
      # plot points and model
      p = ggplot(mapping = aes_string(x = x1n))
      p = p + geom_point(data = data, mapping = aes_string(y = target), size = pointsize)
      p = p + geom_line(data = grid, mapping = aes_string(y = target))
      # show se band
      if (se.band && hasLearnerProperties(learner, "se")) {
        grid$.se = pred.grid$data$se
        grid$.ymin = grid[, target] - grid$.se
        grid$.ymax = grid[, target] + grid$.se
        p = p + geom_ribbon(data = grid, mapping = aes_string(ymin = ".ymin", ymax = ".ymax"), alpha = 0.2)
      }
    } else if (taskdim == 2L) {
      #FIXME: color are not scaled correctly? can be improved?
      # plot background from model / grid
      p = ggplot(mapping = aes_string(x = x1n, y = x2n))
      p = p + geom_tile(data = grid, mapping = aes_string(fill = target))
      p = p + scale_fill_gradient2(low = bg.cols[1L], mid = bg.cols[2L], high = bg.cols[3L], space = "Lab")
      # plot point, with circle and interior color for y
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, colour = target),
                         size = pointsize)
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n),
                         size = pointsize, colour = "black", shape = 1)
      # plot point, with circle and interior color for y
      p = p + scale_colour_gradient2(low = bg.cols[1L], mid = bg.cols[2L], high = bg.cols[3L], space = "Lab")
      p  = p + guides(colour = FALSE)
    }
  }
  else if (td$type == "regr" && taskdim == 2L && three.d) {
    require(plotly)
    # reform grid data
    grid.dcast = reshape2::dcast(grid, as.formula(paste(x1n, x2n, sep = "~")), value.var = target)
    # generate 3D plots data list
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = as.matrix(grid.dcast[,-1]))
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
  if (!three.d){
    p = p + ggtitle(title)
    # deal with greyscale
    if (greyscale) {
      p = p + scale_fill_grey()
    }
  }
  return(p)
}