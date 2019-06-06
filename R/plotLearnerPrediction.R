#' @title Visualizes a learning algorithm on a 1D or 2D data set.
#'
#' @description
#' Trains the model for 1 or 2 selected features, then displays it via [ggplot2::ggplot].
#' Good for teaching or exploring models.
#'
#' For classification and clustering, only 2D plots are supported. The data points, the classification and
#' potentially through color alpha blending the posterior probabilities are shown.
#'
#' For regression, 1D and 2D plots are supported. 1D shows the data, the estimated mean and potentially
#' the estimated standard error. 2D does not show estimated standard error,
#' but only the estimated mean via background color.
#'
#' The plot title displays the model id, its parameters, the training performance
#' and the cross-validation performance.
#'
#' @template arg_learner
#' @template arg_task
#' @param features ([character])\cr
#'   Selected features for model.
#'   By default the first 2 features are used.
#' @template arg_measures
#' @param cv (`integer(1)`)\cr
#'   Do cross-validation and display in plot title?
#'   Number of folds. 0 means no CV.
#'   Default is 10.
#' @param ... (any)\cr
#'   Parameters for `learner`.
#' @param gridsize (`integer(1)`)\cr
#'   Grid resolution per axis for background predictions.
#'   Default is 500 for 1D and 100 for 2D.
#' @param pointsize (`numeric(1)`)\cr
#'   Pointsize for ggplot2 [ggplot2::geom_point] for data points.
#'   Default is 2.
#' @param prob.alpha (`logical(1)`)\cr
#'   For classification: Set alpha value of background to probability for
#'   predicted class? Allows visualization of \dQuote{confidence} for prediction.
#'   If not, only a constant color is displayed in the background for the predicted label.
#'   Default is `TRUE`.
#' @param se.band (`logical(1)`)\cr
#'   For regression in 1D: Show band for standard error estimation?
#'   Default is `TRUE`.
#' @param err.mark (`character(1)`):
#'   For classification: Either mark error of the model on the training data (\dQuote{train}) or
#'   during cross-validation (\dQuote{cv}) or not at all with \dQuote{none}.
#'   Default is \dQuote{train}.
#' @param bg.cols (`character(3)`)\cr
#'   Background colors for classification and regression.
#'   Sorted from low, medium to high.
#'   Default is `TRUE`.
#' @param err.col (`character(1)`)\cr
#'   For classification: Color of misclassified data points.
#'   Default is \dQuote{white}
#' @param err.size (`integer(1)`)\cr
#'   For classification: Size of misclassified data points.
#'   Default is `pointsize`.
#' @param greyscale (`logical(1)`)\cr
#'   Should the plot be greyscale completely?
#'   Default is `FALSE`.
#' @template arg_prettynames
#' @return The ggplot2 object.
#' @export
plotLearnerPrediction = function(learner, task, features = NULL, measures, cv = 10L, ...,
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
  td = getTaskDesc(task)

  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features)) {
    # take first or first 2 features as default
    features = if (length(fns) == 1L) fns else fns[1:2]
  } else {
    assertCharacter(features, max.len = 2L)
    assertSubset(features, choices = fns)
  }
  taskdim = length(features)
  if (td$type %in% c("classif", "cluster") && taskdim != 2L) {
    stopf("Classification and clustering: currently only 2D plots supported, not: %i", taskdim)
  }
  if (td$type == "regr" && taskdim %nin% 1:2) {
    stopf("Regression: currently only 1D and 2D plots supported, not: %i", taskdim)
  }

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

  if (td$type == "classif" && err.mark == "cv" && cv == 0L) {
    stopf("Classification: CV must be switched on, with 'cv' > 0, for err.type = 'cv'!")
  }

  # subset to features, set hyperpars
  task = subsetTask(task, features = features)
  learner = setHyperPars(learner, ...)

  # some shortcut names
  target = td$target
  data = getTaskData(task)
  if (td$type != "cluster") {
    y = getTaskTargets(task)
  }
  x1n = features[1L]
  x1 = data[, x1n]

  # predictions
  # if learner supports prob or se, enable it
  if (td$type == "regr" && taskdim == 1L && hasLearnerProperties(learner, "se")) {
    learner = setPredictType(learner, "se")
  }
  if (td$type == "classif" && hasLearnerProperties(learner, "prob")) {
    learner = setPredictType(learner, "prob")
  }
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
    data$.err = if (err.mark == "train") {
      y != yhat
    } else if (err.mark == "cv") {
      y != pred.cv$data[order(pred.cv$data$id), "response"]
    } else {
      TRUE
    }
    if (taskdim == 2L) {
      p = ggplot(grid, aes_string(x = x1n, y = x2n))
      if (hasLearnerProperties(learner, "prob") && prob.alpha) {
        # max of rows is prob for selected class
        prob = apply(getPredictionProbabilities(pred.grid, cl = td$class.levels), 1, max)
        grid$.prob.pred.class = prob
        p = p + geom_raster(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
          show.legend = TRUE) + scale_fill_discrete(drop = FALSE)
        p = p + scale_alpha(limits = range(grid$.prob.pred.class))
      } else {
        p = p + geom_raster(mapping = aes_string(fill = target))
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
      p = p + guides(alpha = FALSE)
    }
  } else if (td$type == "cluster") {
    if (taskdim == 2L) {
      data$response = factor(yhat)
      p = ggplot(data, aes_string(x = x1n, y = x2n, col = "response"))
      p = p + geom_point(size = pointsize)
    }
  } else if (td$type == "regr") {
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
      # FIXME: color are not scaled correctly? can be improved?
      # plot background from model / grid
      p = ggplot(mapping = aes_string(x = x1n, y = x2n))
      p = p + geom_raster(data = grid, mapping = aes_string(fill = target))
      p = p + scale_fill_gradient2(low = bg.cols[1L], mid = bg.cols[2L], high = bg.cols[3L], space = "Lab")
      # plot point, with circle and interior color for y
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, colour = target),
        size = pointsize)
      p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n),
        size = pointsize, colour = "black", shape = 1)
      # plot point, with circle and interior color for y
      p = p + scale_colour_gradient2(low = bg.cols[1L], mid = bg.cols[2L], high = bg.cols[3L], space = "Lab")
      p = p + guides(colour = FALSE)
    }
  }

  # set title
  if (pretty.names) {
    lrn.str = getLearnerShortName(learner)
  } else {
    lrn.str = getLearnerId(learner)
  }
  title = sprintf("%s: %s", lrn.str, paramValueToString(learner$par.set, learner$par.vals))
  title = sprintf("%s\nTrain: %s; CV: %s", title, perfsToString(perf.train), perfsToString(perf.cv))
  p = p + ggtitle(title)

  # deal with greyscale
  if (greyscale) {
    p = p + scale_fill_grey()
  }
  return(p)
}
