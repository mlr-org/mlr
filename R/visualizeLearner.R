#' Visualizes a learning algorithm on a 1D or 2D data set.
#'
#' Trains the model for 1 or 2 selected features, then displays it via ggplot2.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
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

visualizeLearner = function(learner, task, features = NULL, ..., par.vals = list(),
  gridsize = 50L, pointsize = 2L) {

  learner = checkLearner(learner, type = "classif")
  checkArg(task, "SupervisedTask")
  fns = getTaskFeatureNames(task)
  if (is.null(features))
    features = fns[1:2]
  else
    checkArg(features, subset = fns)
  par.vals = insert(list(...), par.vals)
  gridsize = convertInteger(gridsize)
  checkArg(gridsize, "integer", len = 1L, na.ok = FALSE)
  pointsize = convertInteger(pointsize)
  checkArg(pointsize, "integer", len = 1L, na.ok = FALSE)
  requirePackages("ggplot2", why = "visualizeLearner")

  # subset to features, train learner with selected hyperpars
  task = subsetTask(task, features = features)
  learner = setHyperPars(learner, par.vals = par.vals)
  mod = train(learner, task)
  cv = crossval(learner, task, iters = 10L, show.info = FALSE)

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
  title = sprintf("%s\nTrain err = %g; CV = %g", title, mean(data$.err), cv$aggr[1L])
  p = p + ggtitle(title)

  return(p)
}




