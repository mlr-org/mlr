#' Visualizes a learning algorithm on a 1D or 2D data set.
#'
#' Trains the model for 1 or 2 selected features, then displays it via \code{\link[ggplot2]{ggplot}}.
#'
#'
#' @template arg_learner
#' @template arg_task
#' @param features [\code{character}]\cr
#'   Selected features for model.
#'   By default the first 2 features are used.
#' @param ... [any]\cr
#'   Parameters for \code{learner}.
#' @param ... [\code{list}]\cr
#'   Parameters for \code{learner}.
#' @param features [\code{character}]\cr
#'   Selected features for model.
#'   By default the first 2 features are used.
#' @param grid [\code{integer(1)}]\cr
#'   Grid resolution per axis for 2D background.
#'   Default is 50.
#' @param pointsize [\code{integer(1)}]\cr
#'   Pointsize for ggplot2 geom_point for data points.
#'   Default is 2.
#' @return [\code{\link{WrappedModel}}].
#' @export

visualizeLearner = function(learner, task, features = NULL, measures,  ..., par.vals = list(),
  gridsize = 50L, pointsize = 2L) {

  learner = checkLearner(learner)
  checkArg(task, "SupervisedTask")

  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features)) {
    # take first or first 2 features as default
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else {
    checkArg(features, subset = fns, max.len = 2L)
  }
  taskdim = length(features)

  measures = default.measures(task)
  par.vals = insert(list(...), par.vals)
  gridsize = convertInteger(gridsize)
  checkArg(gridsize, "integer", len = 1L, na.ok = FALSE)
  pointsize = convertInteger(pointsize)
  checkArg(pointsize, "integer", len = 1L, na.ok = FALSE)
  requirePackages("ggplot2", why = "visualizeLearner")

  # subset to features, set hyperpars
  task = subsetTask(task, features = features)
  learner = setHyperPars(learner, par.vals = par.vals)

  # some shortcut names
  td = task$task.desc
  target = td$target
  data = getTaskData(task)
  y = getTaskTargets(task)
  x1n = features[1L]
  x1 = data[, x1n]

  # predictions
  # if learner supports se, enable it
  if (td$type == "regr" && taskdim == 1L && learner$se) {
    learner = setPredictType(learner, "se")
  }
  mod = train(learner, task)
  pred = predict(mod, task)
  perf.train = performance(pred, measures = measures)
  data$.response = pred$data$response
  cv = crossval(learner, task, iters = 10L, measures = measures, show.info = FALSE)
  perf.cv = cv$aggr

  # 2d stuff
  if (taskdim == 2L) {
    x2n = features[2L]
    x2 = data[, x2n]
    # setup data frames for ggplot. grid = background, data = points
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
    colnames(grid) = features
    grid$.response = predict(mod, newdata = grid)$data$response
  }
   print(taskdim)

  if (td$type == "classif") {
    data$.err = data$.response != y
    if (taskdim == 2L) {
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
    }
  } else if (td$type == "regr") {
    if (taskdim == 1L) {
      data$.se = pred$data$se
      data$.ymin = data$.response - data$.se
      data$.ymax = data$.response + data$.se
      p = ggplot(data, aes_string(x = x1n, y = target))
      p = p + geom_point(size = pointsize)
      p = p + geom_line(mapping = aes_string(y = ".response"))
      if (!is.null(data$.se))
        p = p + geom_ribbon(mapping = aes_string(ymin = ".ymin", ymax = ".ymax"), alpha = 0.2)
    } else if (taskdim == 2L) {
      p = ggplot(grid, aes_string(x = x1n, y = x2n, z = ".response", fill = ".response"))
      p = p + geom_tile()
      p = p + scale_fill_gradient2()
      # p = p + geom_contour()
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n),
        pointsize = pointsize)
      # p = ggplot(data, aes_string(x = x1n, y = x2n, z = target))
      # p = p + geom_line(mapping = aes_string(y = ".response"))
    }
  }

  # set title
  title = sprintf("%s: %s", learner$id, paramValueToString(learner$par.set, par.vals))
  title = sprintf("%s\nTrain: %s; CV: %s", title, perfsToString(perf.train), perfsToString(perf.cv))
  p = p + ggtitle(title)

  # p1 = predict(mod, newdata = grid)
  # p2 = predict(mod, newdata = data)
  # grid$.response = p1$data$response
  # data$.response = p2$data$response
  # data$.err = p2$data$response != y


  return(p)
}




