#' @title Visualizes a learning algorithm on a 3D data set.
#'
#' @description
#' Trains the model for 2 or 3 selected features, then displays it via \code{\link[plotly]{plotly}}.
#' Good for teaching or exploring models.
#'
#' For classification, only 3D plots are supported.
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
#'   Default is \code{TRUE}.
#' @param show.legend [\code{logical(1)}]\cr
#'   For classification: Show the legend of right classified points?
#'   Default is \code{TRUE}.
#' @param show.colorbar [\code{logical(1)}]\cr
#'   Show the colorbar?
#'   Default is \code{TRUE}.
#' @param pointsize [\code{numeric(1)}]\cr
#'   Pointsize for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
#'   Default is 2.
#' @param point.col = NULL [\code{character(1)}]\cr
#'   For classification: Set points colors. The color vector muss have either the same length with
#'   the number of classes of response variable, or just a single color.
#'   For regression: Only single color is supported.
#'   Colors are accepted in several different ways, see "Color Specification" section in \code{\link[graphics]{par}}.
#'   Defaul is \code{NULL}.
#' @param point.alpha [\code{numeric(1)}]\cr
#'   For classification: Set the transparancy of prediction point for classification 3D plots with value from 0 to 1.
#'   Default is 1.
#' @param err.mark [\code{character(1)}]:
#'   For classification: Either mark error of the model on the training data (\dQuote{train}) or
#'   during cross-validation (\dQuote{cv}) or not at all with \dQuote{none}.
#'   Default is \dQuote{train}.
#' @param err.size [\code{numeric(1)}]\cr
#'   For classification: Set misclassified point size.
#'   Default is \code{pointsize}.
#' @param err.col [\code{character(1)}]\cr
#'   For classification: Set the colors of missclassified data points.
#'   Default value is \code{NULL} with black color.
#' @param err.alpha [\code{numeric(1)}]\cr
#'   For classification: Set the transparancy of missclassified data points.
#'   Default value is \code{point.alpha}.
#' @template arg_prettynames
#' @param show.bounding [\code{logical(1)}]\cr
#'   For classification: Show the bounding region?
#'   Default is \code{TRUE}.
#' @param bounding.alpha [\code{numeric(1)}]\cr
#'   For \code{show.bounding = TRUE}: Set the transparancy of bounding point.
#'   Default is 0.5.
#' @return The plotly object.
#' @importFrom data.table dcast
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plotly plot_ly add_trace %>% layout toRGB hide_colorbar hide_legend
#' @export
plotLearnerPredictionPlotly = function(learner, task, features = NULL, measures, cv = 10L,  ...,
                                       gridsize, show.point = TRUE, show.legend = TRUE, show.colorbar = TRUE,
                                       pointsize = 2, point.col = NULL, point.alpha = 1,
                                       err.mark = "train", err.size = pointsize, err.col = NULL, err.alpha = point.alpha,
                                       pretty.names = TRUE, show.bounding = TRUE, bounding.alpha = 0.5) {
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

  measures = checkMeasures(measures, task)
  cv = asCount(cv)

  if (missing(gridsize)) {
    gridsize = ifelse(taskdim == 1L, 500, 100)
  } else {
    gridsize = asCount(gridsize)
  }
  assertNumber(pointsize, lower = 0)
  assertChoice(err.mark, choices = c("train", "cv", "none"))

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

    if (!is.null(point.col)){
      if (length(point.col) == length(levels(y)))
        data$.cols = factor(data[, target], levels = levels(data[, target]), labels = point.col)
      else if (length(point.col) == 1L)
        data$.cols = point.col
      else
        warning("point.col must have either the same length with classes of response variable, or just single color!")
    }

    if (!is.null(err.col)){
      if (length(err.col) == length(levels(y)))
        data$.errcols = factor(data[, target], levels = levels(data[, target]), labels = err.col)
      else if (length(err.col) == 1L)
        data$.errcols = err.col
      else
        warning("err.col must have either the same length with classes of response variable, or just a single color!")
    }
    else
      data$.errcols = "black"

    if (taskdim == 2L) {
      cdata = cbind(pred.grid, grid)
      cdata$nresponse = apply(pred.grid$data[, -ncol(pred.grid$data)], 1, max)

      grid.dcast = data.table::dcast(cdata, as.formula(paste(x1n, x2n, sep = "~")), value.var = "nresponse")
      grid.3d = list(x = grid.dcast[,1],
                     y = as.numeric(colnames(grid.dcast)[-1]),
                     z = t(as.matrix(grid.dcast[,-1])))

      p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z,
                  type = "surface", colorbar = list(title = "f(x,y)"), name = "Density")
      if (show.point) {
        data$.z = 0
        if (!is.null(point.col))
          p = add_trace(p, data = data, x = ~get(x1n), y = ~get(x2n), z = ~data$.z,
                        type = "scatter3d", mode = "markers", color = ~get(target),
                        marker = list(size = pointsize, color = toRGB(data$.cols)))
        else
          p = add_trace(p, data = data, x = ~get(x1n), y = ~get(x2n), z = ~data$.z,
                        type = "scatter3d", mode = "markers", color = ~get(target),
                        marker = list(size = pointsize))
      }
      if (!show.legend)
        p = p %>% hide_legend()
      if (!show.colorbar)
        p = p %>% hide_colorbar()
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")),
                                    zaxis = list(title = "z: f(x,y)", range = c(0, 1))),
                       legend = list(xanchor = "right"))
    }
    if (taskdim == 3L) {
      if (show.point) {
        # Plot right classified points
        if (!is.null(point.col))
          p = plot_ly(data = data[!data$.err, ], x = ~get(x1n), y = ~get(x2n), z = ~get(x3n),
                      type = "scatter3d", mode = "markers", color = ~get(target),
                      marker = list(size = pointsize, opacity = point.alpha, color = toRGB(data[!data$.err, ".cols"])))
        else
          p = plot_ly(data = data[!data$.err, ], x = ~get(x1n), y = ~get(x2n), z = ~get(x3n),
                      type = "scatter3d", mode = "markers", color = ~get(target),
                      marker = list(size = pointsize, opacity = point.alpha))


        # Plot missclassified points
        p = add_trace(p, data = data[data$.err, ], x = ~get(x1n), y = ~get(x2n), z = ~get(x3n),
                      type = "scatter3d", mode = "markers", color = ~get(target),
                      marker = list(size = err.size, opacity = err.alpha, color = toRGB(data[data$.err, ".errcols"])))

        if (show.bounding)
          p = add_trace(p, data = grid, x = ~get(x1n), y = ~get(x2n), z = ~get(x3n),
                        type = "mesh3d", color = ~get(target), opacity = bounding.alpha)
      } else {
        if (show.bounding)
          p = plot_ly(data = grid, x = ~get(x1n), y = ~get(x2n), z = ~get(x3n),
                      type = "mesh3d", group = get(target), opacity = bounding.alpha)
      }
      if (!show.legend)
        p = p %>% hide_legend()
      p = p %>% layout(title = title,
                       scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                    yaxis = list(title = paste("y: ", x2n, sep = "")),
                                    zaxis = list(title = paste("z: ", x3n, sep = ""))))
    }
  } else if (td$type == "regr" && taskdim == 2L) {
    # reform grid data
    grid.dcast = data.table::dcast(grid, as.formula(paste(x1n, x2n, sep = "~")), value.var = target)
    # generate 3D plots data list
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = t(as.matrix(grid.dcast[,-1])))

    # plot 3D surface
    p = plot_ly(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z,
                type = "surface",  name = "Learned Value", colorbar = list(title = target))
    # set plot parameters
    p = p %>% layout(title = title,
                     scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                  yaxis = list(title = paste("y: ", x2n, sep = "")),
                                  zaxis = list(title = paste("z: ", target, sep = ""))))
    # add real value trace
    if (show.point) {
      if (is.null(point.col))
        p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target],
                      type = "scatter3d", mode = "markers",
                      marker = list(size = pointsize), name = "Input Value")
      else if (length(point.col) != 1L)
        stop("For regression problem, the point.cols must have the length 1L!")
      else {
        data$.col = point.col
        p = add_trace(p, x = data[, x1n], y = data[, x2n], z = data[, target],
                      type = "scatter3d", mode = "markers",
                      marker = list(size = pointsize, color = toRGB(data$.col)), name = "Input Value")
      }
    }
    if (!show.colorbar)
      p = p %>% hide_colorbar()
  }
  return(p)
}
