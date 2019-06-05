#' @title Generate hyperparameter effect data.
#'
#' @description
#' Generate cleaned hyperparameter effect data from a tuning result or from a
#' nested cross-validation tuning result. The object returned can be used for
#' custom visualization or passed downstream to an out of the box mlr method,
#' [plotHyperParsEffect].
#'
#' @param tune.result ([TuneResult] | [ResampleResult])\cr
#'  Result of [tuneParams] (or [resample] ONLY when used
#'  for nested cross-validation). The tuning result (or results if the
#'  output is from nested cross-validation), also containing the
#'  optimizer results. If nested CV output is passed, each element in the list
#'  will be considered a separate run, and the data from each run will be
#'  included in the dataframe within the returned `HyperParsEffectData`.
#' @param include.diagnostics (`logical(1)`)\cr
#'  Should diagnostic info (eol and error msg) be included?
#'  Default is `FALSE`.
#' @param trafo (`logical(1)`)\cr
#'  Should the units of the hyperparameter path be converted to the
#'  transformed scale? This is only useful when trafo was used to create the
#'  path.
#'  Default is `FALSE`.
#' @param partial.dep (`logical(1)`)\cr
#'  Should partial dependence be requested based on converting to reg task? This
#'  sets a flag so that we know to use partial dependence downstream. This
#'  should most likely be set to `TRUE` if 2 or more hyperparameters were
#'  tuned simultaneously. Partial dependence should always be requested when
#'  more than 2 hyperparameters were tuned simultaneously. Setting to
#'  `TRUE` will cause [plotHyperParsEffect] to automatically
#'  plot partial dependence when called downstream.
#'  Default is `FALSE`.
#'
#' @return (`HyperParsEffectData`)
#'  Object containing the hyperparameter effects dataframe, the tuning
#'  performance measures used, the hyperparameters used, a flag for including
#'  diagnostic info, a flag for whether nested cv was used, a flag for whether
#'  partial dependence should be generated, and the optimization algorithm used.
#'
#' @examples
#' \dontrun{
#' # 3-fold cross validation
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlGrid()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
#'   par.set = ps, control = ctrl)
#' data = generateHyperParsEffectData(res)
#' plt = plotHyperParsEffect(data, x = "C", y = "mmce.test.mean")
#' plt + ylab("Misclassification Error")
#'
#' # nested cross validation
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlGrid()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' lrn = makeTuneWrapper("classif.ksvm", control = ctrl,
#'   resampling = rdesc, par.set = ps)
#' res = resample(lrn, task = pid.task, resampling = cv2,
#'   extract = getTuneResult)
#' data = generateHyperParsEffectData(res)
#' plotHyperParsEffect(data, x = "C", y = "mmce.test.mean", plot.type = "line")
#' }
#' @export
#' @importFrom utils type.convert
generateHyperParsEffectData = function(tune.result, include.diagnostics = FALSE,
  trafo = FALSE, partial.dep = FALSE) {

  assert(
    checkClass(tune.result, "ResampleResult"),
    checkClass(tune.result, classes = "TuneResult")
  )
  assertFlag(include.diagnostics)
  assertFlag(partial.dep)

  # in case we have nested CV
  if (getClass1(tune.result) == "ResampleResult") {
    d = getNestedTuneResultsOptPathDf(tune.result, trafo = trafo)
    num.hypers = length(tune.result$extract[[1]]$x)
    if ((num.hypers > 2) && !partial.dep) {
      stopf("Partial dependence must be requested with partial.dep when tuning more than 2 hyperparameters")
    }
    for (hyp in 1:num.hypers) {
      if (!is.numeric(d[, hyp])) {
        d[, hyp] = type.convert(as.character(d[, hyp]))
      }
    }
    # rename to be clear this denotes the nested cv
    names(d)[names(d) == "iter"] = "nested_cv_run"

    # items for object
    measures = tune.result$extract[[1]]$opt.path$y.names
    hyperparams = names(tune.result$extract[[1]]$x)
    optimization = getClass1(tune.result$extract[[1]]$control)
    nested = TRUE
  } else {
    if (trafo) {
      d = as.data.frame(trafoOptPath(tune.result$opt.path))
    } else {
      d = as.data.frame(tune.result$opt.path)
    }
    # what if we have numerics that were discretized upstream
    num.hypers = length(tune.result$x)
    if ((num.hypers > 2) && !partial.dep) {
      stopf("Partial dependence must be requested with partial.dep when tuning more than 2 hyperparameters")
    }
    for (hyp in 1:num.hypers) {
      if (!is.numeric(d[, hyp])) {
        d[, hyp] = type.convert(as.character(d[, hyp]))
      }
    }
    measures = tune.result$opt.path$y.names
    hyperparams = names(tune.result$x)
    optimization = getClass1(tune.result$control)
    nested = FALSE
  }

  # off by default unless needed by user
  if (include.diagnostics == FALSE) {
    d = within(d, rm("eol", "error.message"))
  }

  # users might not know what dob means, so let's call it iteration
  names(d)[names(d) == "dob"] = "iteration"

  makeS3Obj("HyperParsEffectData", data = d, measures = measures,
    hyperparams = hyperparams,
    diagnostics = include.diagnostics,
    optimization = optimization,
    nested = nested,
    partial = partial.dep)
}

#' @export
print.HyperParsEffectData = function(x, ...) {
  catf("HyperParsEffectData:")
  catf("Hyperparameters: %s", collapse(x$hyperparams))
  catf("Measures: %s", collapse(x$measures))
  catf("Optimizer: %s", collapse(x$optimization))
  catf("Nested CV Used: %s", collapse(x$nested))
  if (x$partial) {
    print("Partial dependence requested")
  }
  catf("Snapshot of data:")
  print(head(x$data))
}

#' @title Plot the hyperparameter effects data
#'
#' @description
#' Plot hyperparameter validation path. Automated plotting method for
#' `HyperParsEffectData` object. Useful for determining the importance
#' or effect of a particular hyperparameter on some performance measure and/or
#' optimizer.
#'
#' @param hyperpars.effect.data (`HyperParsEffectData`)\cr
#'  Result of [generateHyperParsEffectData]
#' @param x (`character(1)`)\cr
#'  Specify what should be plotted on the x axis. Must be a column from
#'  `HyperParsEffectData$data`. For partial dependence, this is assumed to
#'  be a hyperparameter.
#' @param y (`character(1)`)\cr
#'  Specify what should be plotted on the y axis. Must be a column from
#'  `HyperParsEffectData$data`
#' @param z (`character(1)`)\cr
#'  Specify what should be used as the extra axis for a particular geom. This
#'  could be for the fill on a heatmap or color aesthetic for a line. Must be a
#'  column from `HyperParsEffectData$data`. Default is `NULL`.
#' @param plot.type (`character(1)`)\cr
#'  Specify the type of plot: \dQuote{scatter} for a scatterplot, \dQuote{heatmap} for a
#'  heatmap, \dQuote{line} for a scatterplot with a connecting line, or \dQuote{contour} for a
#'  contour plot layered ontop of a heatmap.
#'  Default is \dQuote{scatter}.
#' @param loess.smooth (`logical(1)`)\cr
#'  If `TRUE`, will add loess smoothing line to plots where possible. Note that
#'  this is probably only useful when `plot.type` is set to either
#'  \dQuote{scatter} or \dQuote{line}. Must be a column from
#'  `HyperParsEffectData$data`. Not used with partial dependence.
#'  Default is `FALSE`.
#' @param facet (`character(1)`)\cr
#'  Specify what should be used as the facet axis for a particular geom. When
#'  using nested cross validation, set this to \dQuote{nested_cv_run} to obtain a facet
#'  for each outer loop. Must be a column from `HyperParsEffectData$data`.
#'  Please note that facetting is not supported with partial dependence plots!
#'  Default is `NULL`.
#' @param global.only (`logical(1)`)\cr
#'  If `TRUE`, will only plot the current global optima when setting
#'  x = "iteration" and y as a performance measure from
#'  `HyperParsEffectData$measures`. Set this to FALSE to always plot the
#'  performance of every iteration, even if it is not an improvement. Not used
#'  with partial dependence.
#'  Default is `TRUE`.
#' @param interpolate ([Learner] | `character(1)`)\cr
#'  If not `NULL`, will interpolate non-complete grids in order to visualize a more
#'  complete path. Only meaningful when attempting to plot a heatmap or contour.
#'  This will fill in \dQuote{empty} cells in the heatmap or contour plot. Note that
#'  cases of irregular hyperparameter paths, you will most likely need to use
#'  this to have a meaningful visualization. Accepts either a regression \link{Learner}
#'  object or the learner as a string for interpolation. This cannot be used with partial
#'  dependence.
#'  Default is `NULL`.
#' @param show.experiments (`logical(1)`)\cr
#'  If `TRUE`, will overlay the plot with points indicating where an experiment
#'  ran. This is only useful when creating a heatmap or contour plot with
#'  interpolation so that you can see which points were actually on the
#'  original path. Note: if any learner crashes occurred within the path, this
#'  will become `TRUE`. Not used with partial dependence.
#'  Default is `FALSE`.
#' @param show.interpolated (`logical(1)`)\cr
#'  If `TRUE`, will overlay the plot with points indicating where interpolation
#'  ran. This is only useful when creating a heatmap or contour plot with
#'  interpolation so that you can see which points were interpolated. Not used
#'  with partial dependence.
#'  Default is `FALSE`.
#' @param nested.agg (`function`)\cr
#'  The function used to aggregate nested cross validation runs when plotting 2
#'  hyperparameters. This is also used for nested aggregation in partial
#'  dependence.
#'  Default is `mean`.
#' @param partial.dep.learn ([Learner] | `character(1)`)\cr
#'  The regression learner used to learn partial dependence. Must be specified if
#'  \dQuote{partial.dep} is set to `TRUE` in
#'  [generateHyperParsEffectData]. Accepts either a \link{Learner}
#'  object or the learner as a string for learning partial dependence.
#'  Default is `NULL`.
#' @template ret_gg2
#'
#' @note Any NAs incurred from learning algorithm crashes will be indicated in
#' the plot (except in the case of partial dependence) and the NA values will be
#' replaced with the column min/max depending on the optimal values for the
#' respective measure. Execution time will be replaced with the max.
#' Interpolation by its nature will result in predicted values for the
#' performance measure. Use interpolation with caution. If \dQuote{partial.dep}
#' is set to `TRUE` in [generateHyperParsEffectData], only
#' partial dependence will be plotted.
#'
#' Since a ggplot2 plot object is returned, the user can change the axis labels
#' and other aspects of the plot using the appropriate ggplot2 syntax.
#'
#' @export
#'
#' @examples
#' # see generateHyperParsEffectData
plotHyperParsEffect = function(hyperpars.effect.data, x = NULL, y = NULL,
  z = NULL, plot.type = "scatter", loess.smooth = FALSE, facet = NULL,
  global.only = TRUE, interpolate = NULL, show.experiments = FALSE,
  show.interpolated = FALSE, nested.agg = mean, partial.dep.learn = NULL) {

  assertClass(hyperpars.effect.data, classes = "HyperParsEffectData")
  assertChoice(x, choices = names(hyperpars.effect.data$data))
  assertChoice(y, choices = names(hyperpars.effect.data$data))
  assertSubset(z, choices = names(hyperpars.effect.data$data))
  assertChoice(plot.type, choices = c("scatter", "line", "heatmap", "contour"))
  assertFlag(loess.smooth)
  assertSubset(facet, choices = names(hyperpars.effect.data$data))
  assertFlag(global.only)
  assert(checkClass(interpolate, "Learner"), checkString(interpolate),
    checkNull(interpolate))
  # assign learner for interpolation
  if (checkClass(interpolate, "Learner") == TRUE ||
    checkString(interpolate) == TRUE) {
    lrn = checkLearner(interpolate, "regr")
  }
  assertFlag(show.experiments)
  assertFunction(nested.agg)
  # assign learner for partial dep
  assert(checkClass(partial.dep.learn, "Learner"), checkString(partial.dep.learn),
    checkNull(partial.dep.learn))
  if (checkClass(partial.dep.learn, "Learner") == TRUE ||
    checkString(partial.dep.learn) == TRUE) {
    lrn = checkLearner(partial.dep.learn, "regr")
  }
  if (!is.null(partial.dep.learn) && !is.null(interpolate)) {
    stopf("partial.dep.learn and interpolate can't be simultaneously requested!")
  }
  if (length(x) > 1 || length(y) > 1 || length(z) > 1 || length(facet) > 1) {
    stopf("Greater than 1 length x, y, z or facet not yet supported")
  }

  d = hyperpars.effect.data$data
  if (hyperpars.effect.data$nested) {
    d$nested_cv_run = as.factor(d$nested_cv_run)
  }

  # gather names
  hypers = hyperpars.effect.data$hyperparams
  measures = hyperpars.effect.data$measures

  # set flags for building plots
  na.flag = anyMissing(d[, hyperpars.effect.data$measures])
  z.flag = !is.null(z)
  facet.flag = !is.null(facet)
  heatcontour.flag = plot.type %in% c("heatmap", "contour")
  partial.flag = hyperpars.effect.data$partial
  facet.nested = !is.null(facet) && facet == "nested_cv_run" && !partial.flag

  if (partial.flag && is.null(partial.dep.learn)) {
    stopf("Partial dependence requested but partial.dep.learn not specified!")
  }

  # deal with NAs where optimizer failed
  if (na.flag) {
    d$learner_status = ifelse(is.na(d[, "exec.time"]), "Failure", "Success")
    for (col in hyperpars.effect.data$measures) {
      col.name = stri_split_fixed(col, ".test.mean", omit_empty = TRUE)[[1]]
      if (heatcontour.flag) {
        d[, col][is.na(d[, col])] = get(col.name)$worst
      } else {
        if (get(col.name)$minimize) {
          d[, col][is.na(d[, col])] = max(d[, col], na.rm = TRUE)
        } else {
          d[, col][is.na(d[, col])] = min(d[, col], na.rm = TRUE)
        }
      }
    }
    d$exec.time[is.na(d$exec.time)] = max(d$exec.time, na.rm = TRUE)
  } else {
    # in case the user wants to show this despite no learner crashes
    # Note: ignored for partial dep
    d$learner_status = "Success"
  }

  # we need to work differently depending on if we have partial dependence
  if (partial.flag && !("iteration" %in% c(x, y, z))) {
    # collapse nested for partial dep input
    if (hyperpars.effect.data$nested) {
      averaging = d[, !(names(d) %in% c("iteration", "nested_cv_run",
        hyperpars.effect.data$hyperparams, "eol",
        "error.message", "learner_status")), drop = FALSE]
      hyperpars = lapply(d[, hyperpars.effect.data$hyperparams], "[")
      d = aggregate(averaging, hyperpars, nested.agg)
    }
    partial.task = makeRegrTask(id = "par_dep",
      data = d[, c(hypers, measures[1])], target = measures[1])
    partial.fit = train(lrn, partial.task)
    if ((length(x) == 1) && (length(y) == 1) && !(z.flag)) {
      # we only care about each feature by itself for this case
      d = generatePartialDependenceData(partial.fit, partial.task, x)$data
    } else if ((length(x) == 1) && (length(y) == 1) && (z.flag)) {
      # we need a grid if using more than 1 axis for hyperpars
      d = generatePartialDependenceData(partial.fit, partial.task,
        interaction = TRUE)$data
      # need to aggregate grid
      averaging = d[, c(hyperpars.effect.data$measures[1]), with = FALSE]
      combined.hypers = c(hyperpars.effect.data$hyperparams, x, y, z)
      used.hypers = combined.hypers[duplicated(combined.hypers)]
      hyperpars = lapply(d[, used.hypers, with = FALSE], "[")
      d = aggregate(averaging, hyperpars, mean)
    }
  } else {
    # assign for global only
    if (global.only && x == "iteration" && y %in% hyperpars.effect.data$measures) {
      for (col in hyperpars.effect.data$measures) {
        col.name = stri_split_fixed(col, ".test.mean", omit_empty = TRUE)[[1]]
        if (get(col.name)$minimize) {
          d[, col] = cummin(d[, col])
        } else {
          d[, col] = cummax(d[, col])
        }
      }
    }

    if ((!is.null(interpolate)) && z.flag && (heatcontour.flag)) {
      # create grid
      xo = seq(min(d[, x]), max(d[, x]), length.out = 100)
      yo = seq(min(d[, y]), max(d[, y]), length.out = 100)
      grid = expand.grid(xo, yo, KEEP.OUT.ATTRS = FALSE)
      names(grid) = c(x, y)

      if (hyperpars.effect.data$nested) {
        d.new = d
        new.d = data.frame()
        # for loop for each nested cv run
        for (run in unique(d$nested_cv_run)) {
          d.run = d.new[d.new$nested_cv_run == run, ]
          regr.task = makeRegrTask(id = "interp", data = d.run[, c(x, y, z)],
            target = z)
          mod = train(lrn, regr.task)
          prediction = predict(mod, newdata = grid)
          grid[, z] = prediction$data[, prediction$predict.type]
          grid$learner_status = "Interpolated Point"
          grid$iteration = NA
          # combine the experiment data with interpolated data
          if (facet.nested) {
            grid$nested_cv_run = run
            combined = rbind(d.run[, c(x, y, z, "learner_status", "iteration",
              "nested_cv_run")], grid)
          } else {
            combined = rbind(d.run[, c(x, y, z, "learner_status",
              "iteration")], grid)
          }
          # combine each loop
          new.d = rbind(new.d, combined)
        }
        grid = new.d
      } else {
        regr.task = makeRegrTask(id = "interp", data = d[, c(x, y, z)], target = z)
        mod = train(lrn, regr.task)
        prediction = predict(mod, newdata = grid)
        grid[, z] = prediction$data[, prediction$predict.type]
        grid$learner_status = "Interpolated Point"
        grid$iteration = NA
        # combine the experiment data with interpolated data
        combined = rbind(d[, c(x, y, z, "learner_status", "iteration")], grid)
        grid = combined
      }
      # remove any values that would extrapolate the z
      grid[grid[, z] < min(d[, z]), z] = min(d[, z])
      grid[grid[, z] > max(d[, z]), z] = max(d[, z])
      d = grid
    }

    if (hyperpars.effect.data$nested && z.flag && !facet.nested) {
      averaging = d[, !(names(d) %in% c("iteration", "nested_cv_run",
        hyperpars.effect.data$hyperparams, "eol",
        "error.message", "learner_status")),
      drop = FALSE]
      # keep experiments if we need it
      if (na.flag || (!is.null(interpolate)) || show.experiments) {
        hyperpars = lapply(d[, c(hyperpars.effect.data$hyperparams,
          "learner_status")], "[")
      } else {
        hyperpars = lapply(d[, hyperpars.effect.data$hyperparams], "[")
      }
      d = aggregate(averaging, hyperpars, nested.agg)
      d$iteration = seq_len(nrow(d))
    }
  }

  # just x, y
  if ((length(x) == 1) && (length(y) == 1) && !(z.flag)) {
    if (hyperpars.effect.data$nested && !partial.flag) {
      plt = ggplot(d, aes_string(x = x, y = y, color = "nested_cv_run"))
    } else {
      plt = ggplot(d, aes_string(x = x, y = y))
    }
    if (na.flag && !partial.flag) {
      plt = plt + geom_point(aes_string(shape = "learner_status",
        color = "learner_status")) +
        scale_shape_manual(values = c("Failure" = 24, "Success" = 0)) +
        scale_color_manual(values = c("red", "black"))
    } else {
      plt = plt + geom_point()
    }
    if (plot.type == "line") {
      plt = plt + geom_line()
    }
    if (loess.smooth) {
      plt = plt + geom_smooth()
    }
    if (facet.flag) {
      plt = plt + facet_wrap(facet)
    }
  } else if ((length(x) == 1) && (length(y) == 1) && (z.flag)) {
    # the data we use depends on if interpolation
    if (heatcontour.flag) {
      if (!is.null(interpolate)) {
        plt = ggplot(data = d[d$learner_status == "Interpolated Point", ],
          aes_string(x = x, y = y, fill = z, z = z)) + geom_raster()
        if (show.interpolated && !(na.flag || show.experiments)) {
          plt = plt + geom_point(aes_string(shape = "learner_status")) +
            scale_shape_manual(values = c("Interpolated Point" = 6))
        }
      } else {
        plt = ggplot(data = d, aes_string(x = x, y = y, fill = z, z = z)) +
          geom_raster()
      }
      if ((na.flag || show.experiments) && !show.interpolated && !partial.flag) {
        plt = plt + geom_point(data = d[d$learner_status %in% c("Success",
          "Failure"), ],
        aes_string(shape = "learner_status"),
        fill = "red") +
          scale_shape_manual(values = c("Failure" = 24, "Success" = 0))
      } else if ((na.flag || show.experiments) && (show.interpolated)) {
        plt = plt + geom_point(data = d, aes_string(shape = "learner_status"),
          fill = "red") +
          scale_shape_manual(values = c("Failure" = 24, "Success" = 0,
            "Interpolated Point" = 6))
      }
      if (plot.type == "contour") {
        plt = plt + geom_contour()
      }
      plt = plt + scale_fill_gradientn(colors = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")) # RColorBrewer::brewer.pal(11, "Spectral")
    } else {
      plt = ggplot(d, aes_string(x = x, y = y, color = z))
      if (na.flag) {
        plt = plt + geom_point(aes_string(shape = "learner_status",
          color = "learner_status")) +
          scale_shape_manual(values = c("Failure" = 24, "Success" = 0)) +
          scale_color_manual(values = c("red", "black"))
      } else {
        plt = plt + geom_point()
      }
      if (plot.type == "line") {
        plt = plt + geom_line()
      }
    }
  }
  if (facet.nested) {
    plt = plt + facet_wrap(as.formula(paste("~", "nested_cv_run")))
  }
  return(plt)
}
