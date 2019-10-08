#' @title Create (spatial) resampling plot objects.
#'
#' @description Visualize partitioning of resample objects with spatial information.
#' @import ggplot2
#' @family plot
#' @author Patrick Schratz
#' @param task [Task] \cr
#'   Task object.
#' @param resample [ResampleResult] or named `list` with (multiple)
#' [ResampleResult]\cr
#'   As returned by [resample].
#' @param crs [integer]\cr
#'   Coordinate reference system (EPSG code number) for the supplied
#'   coordinates in the `Task`.
#' @param datum [integer]\cr
#'   Coordinate reference system which should be used in the resulting map.
#' @param repetitions [integer]\cr
#'   Number of repetitions.
#' @param color.train [character]\cr
#'   Color for train set.
#' @param color.test [character]\cr
#'   Color for test set.
#' @param point.size [integer]\cr
#'   Point size.
#' @param axis.text.size [integer]\cr
#'   Font size of axis labels.
#' @param x.axis.breaks [numeric]\cr
#'   Custom x axis breaks
#' @param y.axis.breaks [numeric]\cr
#'   Custom y axis breaks
#'
#' @return ([list] of `2L` containing (1) multiple `gg`` objects and (2) their
#' corresponding labels.
#'
#' @details
#' If a named list is given to `resample`, names will appear in the title of
#' each fold.
#' If multiple inputs are given to `resample`, these must be named.
#'
#' This function makes a hard cut at five columns of the resulting gridded plot.
#' This means if the `resample` object consists of `folds > 5`, these folds will
#' be put into the new row.
#'
#' For file saving, we recommend to use [cowplot::save_plot].
#'
#' When viewing the resulting plot in RStudio, margins may appear to be
#' different than they really are.
#' Make sure to save the file to disk and inspect the image.
#'
#' When modifying axis breaks, negative values need to be used if the area is
#' located in either the western or southern hemisphere.
#' Use positive values for the northern and eastern hemisphere.
#'
#' @section CRS:
#'
#' The crs has to be suitable for the coordinates stored in the `Task`.
#' For example, if the coordinates are UTM, `crs` should be set to a
#' UTM projection.
#' Due to a limited axis space in the resulting grid (especially on the x-axis),
#' the data will by default projected into a lat/lon projection, specifically
#' EPSG 4326.
#' If other projections are desired for the resulting map, please set argument
#' `datum` accordingly. This argument will be passed onto [ggplot2::coord_sf].
#'
#' @md
#' @examples
#' \donttest{
#' rdesc = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
#' r = resample(makeLearner("classif.qda"), spatial.task, rdesc)
#'
#' ## -------------------------------------------------------------
#' ## single unnamed resample input with 5 folds and 2 repetitions
#' ## -------------------------------------------------------------
#'
#' plots = createSpatialResamplingPlots(spatial.task, r, crs = 32717,
#'   repetitions = 2, x.axis.breaks = c(-79.065, -79.085),
#'   y.axis.breaks = c(-3.970, -4))
#' cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 2,
#'   labels = plots[["Labels"]])
#'
#' ## --------------------------------------------------------------------------
#' ## single named resample input with 5 folds and 1 repetition and 32717 datum
#' ## --------------------------------------------------------------------------
#'
#' plots = createSpatialResamplingPlots(spatial.task, list("Resamp" = r),
#'   crs = 32717, datum = 32717, repetitions = 1)
#' cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 1,
#'   labels = plots[["Labels"]])
#'
#' ## -------------------------------------------------------------
#' ## multiple named resample inputs with 5 folds and 1 repetition
#' ## -------------------------------------------------------------
#'
#' rdesc1 = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
#' r1 = resample(makeLearner("classif.qda"), spatial.task, rdesc1)
#' rdesc2 = makeResampleDesc("RepCV", folds = 5, reps = 4)
#' r2 = resample(makeLearner("classif.qda"), spatial.task, rdesc2)
#'
#' plots = createSpatialResamplingPlots(spatial.task,
#'   list("SpRepCV" = r1, "RepCV" = r2), crs = 32717, repetitions = 1,
#'   x.axis.breaks = c(-79.055, -79.085), y.axis.breaks = c(-3.975, -4))
#' cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 2,
#'   labels = plots[["Labels"]])
#'
#' ## -------------------------------------------------------------------------------------
#' ## Complex arrangements of multiple named resample inputs with 5 folds and 1 repetition
#' ## -------------------------------------------------------------------------------------
#'
#' p1 = plot_grid(plist[["Plots"]][[1]], plist[["Plots"]][[2]],
#'   plist[["Plots"]][[3]], ncol = 3, nrow = 1, labels = plist[["Labels"]][1:3],
#'   label_size = 18)
#' p12 = plot_grid(plist[["Plots"]][[4]], plist[["Plots"]][[5]], ncol = 2,
#'   nrow = 1, labels = plist[["Labels"]][4:5], label_size = 18)
#'
#' p2 = plot_grid(plist[["Plots"]][[6]], plist[["Plots"]][[7]],
#'   plist[["Plots"]][[8]], ncol = 3, nrow = 1, labels = plist[["Labels"]][6:8],
#'   label_size = 18)
#' p22 = plot_grid(plist[["Plots"]][[9]], plist[["Plots"]][[10]], ncol = 2,
#'   nrow = 1, labels = plist[["Labels"]][9:10], label_size = 18)
#'
#' cowplot::plot_grid(p1, p12, p2, p22, ncol = 1)
#' }
#' @export
createSpatialResamplingPlots = function(task = NULL, resample = NULL, crs = NULL,
  datum = 4326, repetitions = 1, color.train = "#0072B5", color.test = "#E18727",
  point.size = 0.5, axis.text.size = 14, x.axis.breaks = waiver(),
  y.axis.breaks = waiver()) {

  requireNamespace("hrbrthemes", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)

  # some checks
  if (is.null(crs)) {
    stopf("Please specify a crs that matches the coordinates of the task.")
  }
  if (task$task.desc$has.coordinates == FALSE) {
    stopf("The supplied task needs to have coordinates.")
  }
  if (!identical(as.integer(rownames(task$env$data)), 1:length(task$env$data[, 1]))) {
    rownames(task$env$data) = seq(1:length(task$env$data[, 1]))
  }

  # in case one supplies only one resample object, wrap it into a list
  # to work with map()
  if (!class(resample)[1] == "list") {
    resample = list(resample)
  }
  # how many resamp objects do we have?
  n.resamp = length(resample)

  if (n.resamp > 1 && is.null(names(resample))) {
    length.n.resamp = length(resample)
    names(resample) = seq_len(length.n.resamp)
  }

  # create plot list with length = folds
  nfolds = resample[[1]]$pred$instance$desc$folds

  plot.list.out.all = lapply(resample, function(r) {

    # bind coordinates to data
    data = cbind(task$env$data, task$coordinates)

    # create 'sf' object
    data = sf::st_as_sf(data, coords = names(task$coordinates), crs = crs)

    # create plot list with length = folds
    plot.list = rep(list(data), nfolds * repetitions)

    plot.list.out = imap(plot.list, function(.x, .y) {
      ggplot(.x) +
        geom_sf(data = subset(.x, as.integer(rownames(.x)) %in%
          r$pred$instance[["train.inds"]][[.y]]),
        color = color.train, size = point.size, ) +
        geom_sf(data = subset(.x, as.integer(rownames(.x)) %in%
          r$pred$instance[["test.inds"]][[.y]]),
        color = color.test, size = point.size) +
        scale_x_continuous(breaks = x.axis.breaks) +
        scale_y_continuous(breaks = y.axis.breaks) +
        coord_sf(datum = sf::st_crs(datum)) +
        hrbrthemes::theme_ipsum_rc() +
        theme(axis.text.x = element_text(size = axis.text.size),
          axis.text.y = element_text(size = axis.text.size),
          plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"))
    })
    return(plot.list.out)
  })

  plot.list = unlist(plot.list.out.all, recursive = FALSE)

  # more than 1 repetition?
  if (repetitions > 1) {
    labels = c(length = nfolds * repetitions)
    nfolds.reps = rep(seq_len(nfolds), repetitions)
    reps.nfolds = vector()
    names.resample = vector()
    for (i in seq_len(repetitions)) {
      reps.nfolds = c(reps.nfolds, rep(i, nfolds))
      if (!is.null(names(resample))) {
        names.resample = c(names.resample, rep(names(resample)[i],
          nfolds * repetitions))
      }
    }
    # account for multiple resamp objects
    if (n.resamp > 1) {
      labels = rep(rep(sprintf("[%s] Fold %s (Rep %s)", names.resample,
        nfolds.reps, reps.nfolds)), n.resamp)
    } else {
      if (!is.null(names(resample))) {
        labels = sprintf("[%s] Fold %s (Rep %s)",
          rep(names(resample), nfolds * repetitions),
          seq_len(nfolds), reps.nfolds)
      } else {
        labels = rep(sprintf("Fold %s (Rep %s)", nfolds.reps, reps.nfolds))
      }
    }
  } else {
    # account for multiple resamp objects
    if (n.resamp > 1) {
      names.resample = vector()
      for (i in seq_len(length(names(resample)))) {
        names.resample = c(names.resample, rep(names(resample)[i], nfolds))
      }
      labels = sprintf("[%s] Fold %s", names.resample, seq_len(nfolds))
      labels = rep(labels, n.resamp)
    } else {
      if (!is.null(names(resample))) {
        labels = sprintf("[%s] Fold %s", rep(names(resample), nfolds),
          seq_len(nfolds))
      } else {
        labels = sprintf("Fold %s", seq_len(nfolds))
      }
    }
  }
  return(invisible(list("Plots" = plot.list, "Labels" = labels)))
}
