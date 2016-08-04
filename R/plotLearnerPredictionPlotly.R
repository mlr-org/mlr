#' @title Visualizes a learning algorithm on a 3D data set.
#'
#' @description
#' Trains the model for 2 or 3 selected features, then displays it via \code{\link[plotly]{plotly}}.
#' Good for teaching or exploring models.
#'
#' For classification, only 3D plots are supported. The separating area will be displayed 
#' in three ways "region", "bounding.point" and "bounding.region"
#'
#' For regression, only 3D plots are supported. 
#'
#' The plot title displays the model id, its parameters, the training performance
#' and the cross-validation performance.
#'
#' @template arg_learner
#' @template arg_task
#' @param features [\code{character}]\cr
#'   Selected features for model.
#'   By default the first 2 features are used.
#' @template arg_measures
#' @param cv [\code{integer(1)}]\cr
#'   Do cross-validation and display in plot title?
#'   Number of folds. 0 means no CV.
#'   Default is 10.
#' @param ... [any]\cr
#'   Parameters for \code{learner}.
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution per axis for background predictions.
#'   Default is 500 for 1D and 100 for 2D.
#' @param show.point [\code{logical(1)}]\cr
#'   Show the input data point?
#'   Default is \code{TRUE}
#' @param show.point.legend [\code{logical(1)}]\cr
#'   For classification: Show the point legend?
#'   Default is \code{TRUE}
#' @param show.colorbar [\code{logical(1)}]\cr
#'   Show the colorbar?
#'   Default is \code{TRUE}
#' @param pointsize [\code{numeric(1)}]\cr
#'   Pointsize for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
#'   Default is 2.
#' @param err.mark [\code{character(1)}]:
#'   For classification: Either mark error of the model on the training data (\dQuote{train}) or
#'   during cross-validation (\dQuote{cv}) or not at all with \dQuote{none}.
#'   Default is \dQuote{train}.
#' @param err.col [\code{character(1)}]\cr
#'   For classification: Color of misclassified data points.
#'   Default value is \dQuote{black}
#' @param regr.greyscale [\code{logical(1)}]\cr
#'   For regression: Should the plot be greyscale completely?
#'   Default is \code{FALSE}.
#' @template arg_prettynames
#' @param point.alpha [\code{numeric(1)}]\cr
#'   For classification: Set the transparancy of prediction point for classification 3D plots with value from 0 to 1.
#'   Default is 1.
#' @param show [\code{character(1)}]\cr
#'   For classification: Set the separating method. 3 Possiable values: "bounding.point", "bounding.region" and "region".
#'   Default is \code{NULL}
#' @param bounding.alpha [\code{numeric(1)}]\cr
#'   For \code{show = "bounding.point"}: Set the transparancy of bounding point.
#'   Default is 0.5.
#' @param bounding.point.size [\code{numeric(1)}]\cr
#'   For \code{show = "bounding.point"}: Set the size of bounding point.
#'   Default is \code{pointsize}.
#' @param bounding.point.legend [\code{logical(1)}]\cr
#'   For \code{show = "bounding.point"}: Show the legend of bounding point?
#'   Default is \code{FALSE}.
#' @param bounding.region.alphahull [\code{integer(1)}]\cr
#'   For \code{show = "bounding.region"}: Set the alpha shapes. See \url{https://plot.ly/python/alpha-shapes/}.
#'   Default is -1.
#' @param region.alpha [\code{numeric(1)}]\cr
#'   For \code{show = "region"}: Set the transparancy of the separating region.
#'   Default is 0.5.
#' @return The plotly object.
#' @import plotly
#' @export
plotLearnerPredictionPlotly = function(learner, task, features = NULL, measures, cv = 10L,  ...,
                                 gridsize, show.point = TRUE, show.point.legend = TRUE, show.colorbar = TRUE,
                                 pointsize = 2, err.mark = "train", err.col = "black",
                                 regr.greyscale = FALSE, pretty.names = TRUE,
                                 point.alpha = 1, show = NULL, bounding.alpha = 0.5, 
                                 bounding.point.size = pointsize, 
                                 bounding.point.legend = FALSE,
                                 bounding.region.alphahull = -1,
                                 region.alpha = 0.5) {
  learner = checkLearner(learner)
  assert(
    checkClass(task, "ClassifTask"),
    checkClass(task, "RegrTask")
  )
  td = getTaskDescription(task)
  
  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (length(fns) == 1L)
    stopf("plotLearnerPrediction() works with at least 2 features.")
  if (is.null(features))
    features = fns[1:2]
  else {
    assertCharacter(features, max.len = 3L)
    assertSubset(features, choices = fns)
  }
  taskdim = length(features)
  if (td$type == "classif" && taskdim %nin% c(2L, 3L))
    stopf("Classification: currently only 2 or 3 features plots supported in plotLearnerPredictionPlotly(), not: %i", taskdim)
  if (td$type == "regr" && taskdim != 2L)
    stopf("Regression: currently only 2 features plots supported in plotLearnerPredictionPlotly(), not: %i", taskdim)
  if (taskdim != 2L && !show.point && missing(show))
    stopf("Either 'show.point' is given TRUE, or 'show' is given a type in ('bounding.point', 'bounding.region', 'region')")
  if (taskdim != 2L && !missing(show) && show %nin% c("bounding.point", "bounding.region", "region"))
    stopf("'show' must be one of ('bounding.point', 'bounding.region', 'region')")
  
  measures = checkMeasures(measures, task)
  cv = asCount(cv)
  
  if (missing(gridsize)) {
    gridsize = ifelse(taskdim == 1L, 500, 100)
  } else {
    gridsize = asCount(gridsize)
  }
  assertNumber(pointsize, lower = 0)
  assertChoice(err.mark, choices = c("train", "cv", "none"))
  assertString(err.col)
  assertLogical(regr.greyscale)
  
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
      seq(min(x1), max(x1), length.out = gridsize / 5),
      seq(min(x2), max(x2), length.out = gridsize / 5),
      seq(min(x3), max(x3), length.out = gridsize / 5)
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
    if (taskdim == 2L) {
      cdata = cbind(pred.grid, grid)
      cdata$nresponse = apply(subset(pred.grid$data, select = -response), 1, max)
      
      grid.dcast = reshape2::dcast(cdata, as.formula(paste(x1n, x2n, sep = "~")), value.var = "nresponse")
      grid.3d = list(x = grid.dcast[,1],
                     y = as.numeric(colnames(grid.dcast)[-1]),
                     z = t(as.matrix(grid.dcast[,-1])))
      
      p = plot_ly(data = grid.3d, x = x, y = y, z = z,
                  type = "surface", showscale = show.colorbar, 
                  colorbar = list(title = target), name = "Density")
      if (show.point) {
        data$.z = 0
        p = add_trace(p, data = data, x = get(x1n), y = get(x2n), z = .z,
                      type = "scatter3d", mode = "markers", symbol = get(target),
                      marker = list(size = pointsize), 
                      showlegend = show.point.legend)
      }
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n)),
                                    yaxis = list(title = paste("y: ", x2n)),
                                    zaxis = list(title = "z: f(x,y)", range = c(0, 1))),
                       legend = list(xanchor = "right"))
    }
    if (taskdim == 3L) {
      if (show.point) {
        if (nlevels(data[, target]) < 3)
          cols = toRGB(c("blue", "red"))
        else
          cols = RColorBrewer::brewer.pal(nlevels(data[, target]), "Set1")
        data$.cols = as.character(factor(as.numeric(data[, target]), labels = cols))
        data[data$.err == TRUE, ".cols"] = toRGB(err.col)
        
        tmp = data
        tmp = tmp[order(tmp$.cols, decreasing = T), ]
        
        p = plot_ly(data = tmp, x = get(x1n), y = get(x2n), z = get(x3n), 
                    type = "scatter3d", mode = "markers", symbol = tmp[, target], 
                    marker = list(size = pointsize, opacity = point.alpha, color = .cols),
                    showlegend = show.point.legend)
        
        if (!missing(show)) {
          if (show == "region")
            p = add_trace(p, data = grid, x = get(x1n), y = get(x2n), z = get(x3n),
                          type = "mesh3d", mode = "markers", opacity = region.alpha,
                          color = grid[, target], alphahull = 0)
          else {
            index = NULL
            for (i in 1:c(nrow(pred.grid$data) - 1)) {
              if (pred.grid$data[i, "response"] != pred.grid$data[i + 1, "response"] && (i %% (gridsize / 5)) != 0) {
                index = append(index, i)
              }
            }
            index = index[!duplicated(index)]
            
            if (show == "bounding.region")
              p = add_trace(p, data = grid[index, ], x = get(x1n), y = get(x2n), z = get(x3n),
                            type = "mesh3d", color = get(target), 
                            alphahull = bounding.region.alphahull, opacity = bounding.alpha)
            else if (show == "bounding.point")
              p = add_trace(p, data = grid[index, ], x = get(x1n), y = get(x2n), z = get(x3n),
                            type = "scatter3d", mode = "markers", opacity = bounding.alpha,
                            marker = list(size = bounding.point.size), color = grid[index, target],
                            showlegend = bounding.point.legend)
          }
        }
      } else {
        if (show == "region")
          p = plot_ly(data = grid, x = get(x1n), y = get(x2n), z = get(x3n),
                      type = "mesh3d", mode = "markers", opacity = region.alpha,
                      color = grid[, target], alphahull = 0)
        else {
          index = NULL
          for (i in 1:c(nrow(pred.grid$data) - 1)) {
            if (pred.grid$data[i, "response"] != pred.grid$data[i + 1, "response"] && (i %% (gridsize / 5)) != 0) {
              index = append(index, i)
            }
          }
          index = index[!duplicated(index)]
          
          if (show == "bounding.region")
            p = plot_ly(data = grid[index, ], x = get(x1n), y = get(x2n), z = get(x3n),
                        type = "mesh3d", color = grid[index, target], 
                        alphahull = bounding.region.alphahull, opacity = bounding.alpha)
          else if (show == "bounding.point")
            p = plot_ly(data = grid[index, ], x = get(x1n), y = get(x2n), z = get(x3n),
                        type = "scatter3d", mode = "markers", opacity = bounding.alpha,
                        marker = list(size = bounding.point.size), color = grid[index, target],
                        showlegend = bounding.point.legend)
        }
      }
    }
  } else if (td$type == "regr" && taskdim == 2L) {
    # reform grid data
    grid.dcast = reshape2::dcast(grid, as.formula(paste(x1n, x2n, sep = "~")), value.var = target)
    # generate 3D plots data list
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = t(as.matrix(grid.dcast[,-1])))
    
    if (regr.greyscale) {
      # plot 3D surface
      p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, 
                  type = "surface", colorbar = list(title = target), showscale = show.colorbar,
                  name = "Learned Value", colorscale = "Greys")
      # set plot parameters
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                    zaxis = list(title = paste("z: ", target, sep = ""))))
      # add real value trace
      if (show.point) {
        p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target], 
                      type = "scatter3d", color = data[, target], mode = "markers",
                      marker = list(size = pointsize, colorbar = F, colorscale = "Greys"), 
                      name = "Input Value", showscale = F)
      }
    } else {
      # plot 3D surface
      p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, 
                  type = "surface", showscale = show.colorbar, 
                  colorbar = list(title = target), name = "Learned Value")
      # set plot parameters
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                    zaxis = list(title = paste("z: ", target, sep = ""))))
      # add real value trace
      if (show.point) {
        p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target], 
                      type = "scatter3d", color = data[, target], mode = "markers",
                      marker = list(size = pointsize, colorbar = F), 
                      name = "Input Value", showscale = F)
      }
    }
  }
  return(p)
}