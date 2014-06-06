#FIXME: the name of the method might be improved. it is too unspecific

#' @title Visualizes a learning algorithm on a 1D or 2D data set.
#'
#' @description
#' Trains the model for 1 or 2 selected features, then displays it via \code{\link[ggplot2]{ggplot}}.
#' Good for teaching or exploring models.
#'
#' For classification, only 2D plots are supported. The data points, the classification and
#' potentially through color alpha blending the posterior probabilities are shown.
#'
#' For regression, 1D and 2D plots are supported. 1D shows the data, the estimated mean and potentially
#' the estimated standard error. 2D does not show estimated standard error,
#' but only the estimated mean via background color.
#'
#' The plot title displays the model id, its parameters, the test training performance
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
#' @param pointsize [\code{numeric(1)}]\cr
#'   Pointsize for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
#'   Default is 2.
#' @param prob.alpha [\code{logical(1)}]\cr
#'   For classification: Set alpha value of background to probability for
#'   predicted class? Allows visualization of \dQuote{confidence} for prediction.
#'   If not, only a constant color is displayed in the background for the predicted label.
#'   Default is \code{TRUE}.
#' @param se.band [\code{logical(1)}]\cr
#'   For regression in 1D: Show band for standard error estimation?
#'   Default is \code{TRUE}.
#' @param err.mark [\code{character(1)}]:
#'   For classification: Either mark error of the model on the training data (\dQuote{train}) or
#'   during cross-validation (\dQuote{cv}) or not at all with \dQuote{none}.
#'   Default is \dQuote{train}.
#' @param err.col [\code{character(1)}]\cr
#'   For classification: Color of misclassified data points.
#'   Default is \dQuote{orange}
#' @return The ggplot2 object.
#' @export
visualizeLearner = function(learner, task, features = NULL, measures, cv = 10L,  ...,
  gridsize, pointsize = 2,
  prob.alpha = TRUE, se.band = TRUE,
  err.mark = "train", err.col = "orange") {

  learner = checkLearner(learner)
  checkArg(task, "SupervisedTask")
  td = task$task.desc

  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features)) {
    # take first or first 2 features as default
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else {
    checkArg(features, subset = fns, max.len = 2L)
  }
  taskdim = length(features)
  if (td$type == "classif" && taskdim != 2L)
    stopf("Classification: currently only 2D plots supported, not: %i", taskdim)
  if (td$type == "regr" && taskdim %nin% 1:2)
    stopf("Regression: currently only 1D and 2D plots supported, not: %i", taskdim)

  measures = checkMeasures(measures, task)
  cv = convertInteger(cv)
  checkArg(cv, "integer", len = 1L, lower = 0L, na.ok = FALSE)

  if (missing(gridsize)) {
    gridsize = ifelse(taskdim == 1L, 500, 100)
  } else {
    gridsize = convertInteger(gridsize)
    checkArg(gridsize, "integer", len = 1L, na.ok = FALSE)
  }
  checkArg(pointsize, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(prob.alpha, "logical", len = 1L, na.ok = FALSE)
  checkArg(se.band, "logical", len = 1L, na.ok = FALSE)
  checkArg(err.mark, choices = c("train", "cv", "none"))
  checkArg(err.col, "character", len = 1L, na.ok = FALSE)
  if (td$type == "classif" && err.mark == "cv" && cv == 0L)
    stopf("Classification: CV must be switched on, with 'cv' > 0, for err.type = 'cv'!")

  requirePackages("ggplot2", why = "visualizeLearner")

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
  if (td$type == "regr" && taskdim == 1L && learner$se)
    learner = setPredictType(learner, "se")
  if (td$type == "classif" && hasProperties(learner, "prob"))
    learner = setPredictType(learner, "prob")
  mod = train(learner, task)
  pred.train = predict(mod, task)
  yhat = pred.train$data$response
  perf.train = performance(pred.train, measures = measures)
  if (cv > 0L) {
    cv = crossval(learner, task, iters = 10L, measures = measures, show.info = FALSE)
    perf.cv = cv$aggr
    pred.cv = cv$pred
  } else {
    perf.cv = NA_real_
  }

  # 2d stuff
  if (taskdim == 2L) {
    x2n = features[2L]
    x2 = data[, x2n]
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
  }
  colnames(grid) = features
  pred.grid = predict(mod, newdata = grid)
  grid[, target] = pred.grid$data$response

  if (td$type == "classif") {
    data$.err = if (err.mark == "train")
      (y != yhat)
    else if (err.mark == "cv")
      y != pred.cv$data[order(pred.cv$data$id), "response"]
    else
      NULL
    if (taskdim == 2L) {
      p = ggplot(grid, aes_string(x = x1n, y = x2n))
      if (learner$prob && prob.alpha) {
        # max of rows is prob for selected class
        grid$.prob.pred.class = apply(getProbabilities(pred.grid, cl = td$class.levels), 1, max)
        p = p + geom_tile(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
          show_guide = TRUE)
        p = p + scale_alpha(range = range(grid$.prob.pred.class))
      } else {
        p = p + geom_tile(mapping = aes_string(fill = target))
      }
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, shape = target),
        size = pointsize)
      if (err.mark != "none" && any(data$.err)) {
        p = p + geom_point(data = subset(data, data$.err),
          mapping = aes_string(x = x1n, y = x2n, shape = target),
          size = pointsize + 1, col = err.col, show_guide = FALSE)
      }
      p  = p + guides(alpha = FALSE)
    }
  } else if (td$type == "regr") {
    if (taskdim == 1L) {
      # plot points and model
      p = ggplot(mapping = aes_string(x = x1n))
      p = p + geom_point(data = data, mapping = aes_string(y = target), size = pointsize)
      p = p + geom_line(data = grid, mapping = aes_string(y = target))
      # show se band
      if (se.band && learner$se) {
        grid$.se = pred.grid$data$se
        grid$.ymin = grid[, target] - grid$.se
        grid$.ymax = grid[, target] + grid$.se
        p = p + geom_ribbon(data = grid, mapping = aes_string(ymin = ".ymin", ymax = ".ymax"), alpha = 0.2)
      }
    } else if (taskdim == 2L) {
      #FIXME: color are not scaled correctly?
      cols = c("red", "white", "blue")
      # plot background from model / grid
      p = ggplot(mapping = aes_string(x = x1n, y = x2n))
      p = p + geom_tile(data = grid, mapping = aes_string(fill = target))
      p = p + scale_fill_gradient2(low = cols[1L], mid = cols[2L], high = cols[3L])
      # plot point, with circle and interior color for y
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, colour = target),
        size = pointsize)
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n),
        size = pointsize, colour = "black", shape = 1)
      # plot point, with circle and interior color for y
      p = p + scale_colour_gradient2(low = cols[1L], mid = cols[2L], high = cols[3L])
      p  = p + guides(colour = FALSE)
    }
  }

  # set title
  title = sprintf("%s: %s", learner$id, paramValueToString(learner$par.set, learner$par.vals))
  title = sprintf("%s\nTrain: %s; CV: %s", title, perfsToString(perf.train), perfsToString(perf.cv))
  p = p + ggtitle(title)
  return(p)
}




