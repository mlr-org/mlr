#' @title Generate partial predictions
#'
#' @description
#' Estimate how the learned prediction function is affected by one or more features
#'
#' @family partial_prediction
#' @family generate_plot_data
#'
#' @param obj a \code{\link{WrappedModel}} returned from \code{\link{train}}.
#' @param data a \code{data.frame} with the same columns as are present in the training data.
#' @param features \code{character}\cr
#'   A vector of feature names matching the training data.
#' @param interaction \code{logical(1)}\cr
#'   Whether the \code{features} should be interacted or not. If \code{TRUE} then the Cartesian product of the
#'   prediction grid for each feature is taken, and the partial prediction at each unique combination of
#'   values of the features is estimated. Note that if the length of \code{features} is greater than two,
#'   \code{\link{plotPartialPrediction}} and \code{\link{plotPartialPredictionGGVIS}} cannot be used.
#'   If \code{FALSE} each feature is considered separately. In this case \code{features} can be much longer
#'   than two.
#'   Default is \code{FALSE}.
#' @param fun for regression, a function that accepts a numeric vector and returns either a single number
#'   such as a measure of location such as the mean, or three numbers, which give a lower bound,
#'   a measure of location, and an upper bound. Note if three numbers are returned they must be
#'   in this order. For classification with \code{predict.type = "prob"} the function must accept
#'   a numeric matrix with the number of columns equal to the number of class levels of the target.
#'   For classification with \code{predict.type = "response"} (the default) the function must accept
#'   a character vector and output a numeric vector with length equal to the number of classes in the
#'   target feature.
#'   The default is the mean, unless \code{obj} is classification with \code{predict.type = "response"}
#'   in which case the default is the proportion of observations predicted to be in each class.
#' @param resample \code{character(1)}\cr
#'   Defines how the prediction grid for each feature is created. If \dQuote{bootstrap} then
#'   values are sampled with replacement from the training data. If \dQuote{subsample} then
#'   values are sampled without replacement from the training data. If \dQuote{none} an evenly spaced
#'   grid between either the empirical minimum and maximum, or the minimum and maximum defined by
#'   \code{fmin} and \code{fmax}, is created.
#'   Default is \dQuote{none}.
#' @param fmin \code{numeric}\cr
#'   The minimum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = NULL} and when the empirical minimum is higher
#'   than the theoretical minimum for a given feature.
#'   Default is the empirical minimum of each numeric feature and NA for factor features.
#' @param fmax \code{numeric}\cr
#'   The maximum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = "none"} and when the empirical maximum is lower
#'   than the theoretical maximum for a given feature.
#'   Default is the empirical maximum of each numeric feature and NA for factor features.
#' @param gridsize \code{integer(1)}\cr
#'   The length of the prediction grid created for each feature.
#'   If \code{resample = "bootstrap"} or \code{resample = "subsample"} then this defines
#'   the number of (possibly non-unique) values resampled. If \code{resample = NULL} it defines the
#'   length of the evenly spaced grid created.
#' @param ... additional arguments to be passed to \code{\link{predict}}.
#'
#' @return an object of class \code{PartialPredictionData}, a named list, which contains the data,
#'   the target, the features, and the task description.
#' @examples
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, iris.task)
#' pd = generatePartialPredictionData(fit, getTaskData(iris.task), c("Petal.Width", "Petal.Length"))
#' plotPartialPrediction(pd)
#' @export
generatePartialPredictionData = function(obj, data, features, interaction = FALSE, fun = mean,
                                         resample = "none",
                                         fmin = sapply(features, function(x)
                                           ifelse(!is.factor(data[[x]]), min(data[[x]], na.rm = TRUE), NA)),
                                         fmax = sapply(features, function(x)
                                           ifelse(!is.factor(data[[x]]), max(data[[x]], na.rm = TRUE), NA)),
                                         gridsize = 10L, ...) {
  td = obj$task.desc
  assertClass(obj, "WrappedModel")
  assertDataFrame(data, col.names = "unique", min.rows = 1L, min.cols = length(obj$features) + length(td$target))
  checkColumnNames(data, "data")
  assertSetEqual(colnames(data), c(obj$features, td$target), ordered = FALSE)
  assertSubset(td$target, colnames(data))
  assertSubset(features, obj$features)
  assertFlag(interaction)
  assertFunction(fun)
  assertChoice(resample, c("none", "bootstrap", "subsample"))
  assertNumeric(fmin, finite = TRUE)
  assertNumeric(fmax, finite = TRUE)
  assertCount(gridsize, positive = TRUE)

  rng = vector("list", length(features))
  names(rng) = features
  for (i in 1:length(features))
    rng[[i]] = generateFeatureGrid(features[i], data, resample, fmax[i], fmin[i], gridsize)
  rng = as.data.frame(rng)
  if (length(features) > 1L & interaction)
    rng = expand.grid(rng)

  ## check that function returns input of valid length and type
  test = fun(1:3)
  if (!is.numeric(test))
    stop("fun argument must return a numeric vector")
  if (td$type == "classif" & obj$learner$predict.type == "response" & length(test) != 3L)
    stop("function argument must return a numeric vector with length equal to the number of target class levels.")
  if (td$type == "classif" & obj$learner$predict.type == "prob" & length(test) != 1L)
    stop("function argument must return a numeric vector of length 1.")
  if (td$type == "regr" & !(length(test) %in% c(1L, 3L)))
    stop("function argument must return a numeric vector of length 1 or 3.")

  if (td$type == "regr")
    target = td$target
  else if (td$type == "classif")
    target = td$class.levels
  else
    target = "risk"

  if (length(features) > 1L & !interaction) {
    out = lapply(features, function(x) {
      rng = rng[x][!is.na(rng[x]),, drop = FALSE]
      args = list(obj = obj, data = data, fun = fun, td = td, rng = rng, features = x)
      out = parallelMap::parallelMap(doPartialPredictionIteration, seq_len(nrow(rng)), more.args = args)
      out = as.data.frame(do.call("rbind", out))
      if (td$type == "regr" & length(test) == 3L)
        colnames(out) = c("lower", target, "upper")
      else
        colnames(out) = target
      out[[x]] = rng[[x]]
      out
    })
    out = plyr::ldply(out)
  } else {
    args = list(obj = obj, data = data, fun = fun, td = td, rng = rng, features = features, ...)
    out = parallelMap::parallelMap(doPartialPredictionIteration, seq_len(nrow(rng)), more.args = args)
    out = as.data.frame(do.call("rbind", out))
    if (td$type == "regr" & length(test) == 3L)
      colnames(out) = c("lower", target, "upper")
    else
      colnames(out) = target
    out = cbind(out, rng)
  }

  if (length(test) == 3L & td$type == "regr")
    if (!all(out$lower <= out[[target]] & out[[target]] <= out$upper))
      stop("function argument must return a sorted numeric vector ordered lowest to highest.")

  makeS3Obj("PartialPredictionData",
            data = out,
            task.desc = td,
            target = target,
            features = features,
            interaction = interaction)
}
#' Result of \code{\link{generatePartialPredictionData}}.
#'
#' @family partial_prediction
#'
#' \itemize{
#'   \item{data \code{data.frame}}{Has columns for the prediction (one column for regression and
#'   survival analysis, and a column for each level of the target feature for classification) and
#'   a column for each element of \code{features}.)}
#'   \item{task.desc \code{\link{TaskDesc}}}{Task description}.
#'   \item{target}{Target feature for regression, target feature levels for classification,
#'         survival and event indicator for survival.}
#'   \item{features}{Features argument input}.
#'   \item{interaction}{Whether or not the features were interacted (i.e. conditioning)}
#' }
#' @name PartialPredictionData
#' @rdname PartialPredictionData
NULL
#' @export
print.PartialPredictionData = function(x, ...) {
  catf("PartialPredictionData")
  catf("Task: %s", x$task.desc$id)
  catf("Features: %s", paste(x$features, collapse = ", "))
  catf("Target: %s", paste(x$target, collapse = ", "))
  catf("Interaction: %s", x$interaction)
  print(head(x$data))
}
#' @title Plot a partial prediction with ggplot2
#' @description
#' Plot a partial prediction from \code{\link{generatePartialPredictionData}} using ggplot2.
#'
#' @family partial_prediction
#' @family plot
#'
#' @param obj \code{PartialPredictionData}\cr
#'   Generated by \code{\link{generatePartialPredictionData}}.
#' @param facet \code{character(1)}\cr
#'   The name of a feature to be used for facetting.
#'   This feature must have been an element of the \code{features} argument to
#'   \code{\link{generatePartialPredictionData}} and is only applicable when said argument had length
#'   greater than 1.
#'   If \code{\link{generatePartialPredictionData}} is called with the \code{interaction} argument \code{FALSE}
#'   (the default) with argument \code{features} of length greater than one, then \code{facet} is ignored and
#'   each feature is plotted in its own facet.
#'   Note that if any of the elements of the \code{features} argument of \code{\link{generatePartialPredictionData}}
#'   are factors, they will be coerced to numerics.
#'   Default is \code{NULL}.
#' @return A ggplot2 object.
#' @export
plotPartialPrediction = function(obj, facet = NULL) {
  assertClass(obj, "PartialPredictionData")
  if (obj$interaction & length(obj$features) > 2L)
    stop("It is only possible to plot 2 features with this function.")

  if (!is.null(facet) & obj$interaction & length(obj$features) == 2L) {
    feature = obj$features[which(obj$features != facet)]
    if (!is.factor(obj$data[[facet]]))
      obj$data[[facet]] = paste(facet, "=", as.factor(signif(obj$data[[facet]], 2)), sep = " ")
    else
      obj$data[[facet]] = paste(facet, "=", obj$data[[facet]])
  } else {
    feature = obj$features
    facet = NULL
  }

  target = obj$target

  bounds = all(c("lower", "upper") %in% colnames(obj$data) & obj$task.desc$type %in% c("surv", "regr"))

  if (all(target %in% obj$task.desc$class.levels)) {
    out = reshape2::melt(obj$data, id.vars = obj$features, variable = "Class", value.name = "Probability")
    out$Class = gsub("^prob\\.", "", out$Class)
    if (length(unique(out$Class)) == 2L)
      out = out[out$Class == obj$task.desc$positive, ]
    if (length(feature) > 1L) {
      ## suppress warnings for reshaping vectors of different types
      ## factors are coerced to numeric/integers
      ## not a way to avoid this with facetting since everything is on one scale
      out = reshape2::melt(out, id.vars = c("Class", "Probability"),
                                            variable = "Feature", value.name = "Value")
      out = out[!is.na(out$Value), ]
      if (length(unique(out$Class)) == 2L)
        out = out[out$Class == obj$task.desc$positive, ]
      plt = ggplot2::ggplot(out, ggplot2::aes_string("Value", "Probability", group = "Class", color = "Class"))
      plt = plt + ggplot2::facet_wrap(as.formula("~ Feature"), scales = "free_x")
    } else {
      plt = ggplot2::ggplot(out, ggplot2::aes_string(feature, "Probability", group = "Class", color = "Class"))
    }
  } else {
    if (length(feature) > 1L) {
      if (bounds)
        out = reshape2::melt(obj$data, id.vars = c(target, "lower", "upper"),
                             variable = "Feature", value.name = "Value")
      else
        out = reshape2::melt(obj$data, id.vars = target, variable = "Feature", value.name = "Value")
      out = out[!is.na(out$Value), ]
      plt = ggplot2::ggplot(out, ggplot2::aes_string("Value", target, group = "Feature"))
      plt = plt + ggplot2::facet_wrap(as.formula("~ Feature"), scales = "free_x")
    } else {
      plt = ggplot2::ggplot(obj$data, ggplot2::aes_string(feature, target))
    }
  }
  plt = plt + ggplot2::geom_point() + ggplot2::geom_line()

  if (bounds)
    plt = plt + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "lower", ymax = "upper"), alpha = .5)

  if (!is.null(facet)) {
    plt = plt + ggplot2::facet_wrap(as.formula(paste("~", facet)))
  }

  plt
}
#' @title Plot a partial prediction using ggvis
#' @description
#' Plot a partial prediction from \code{\link{generatePartialPredictionData}} using ggvis.
#'
#' @family partial_prediction
#' @family plot
#'
#' @param obj \code{PartialPredictionData}\cr
#'   Generated by \code{\link{generatePartialPredictionData}}.
#' @param interaction \code{character(1)}\cr
#'   The name of a feature to be mapped to an interactive sidebar using Shiny.
#'   This feature must have been an element of the \code{features} argument to
#'   \code{\link{generatePartialPredictionData}} and is only applicable when said argument had length
#'   greater than 1.
#'   If \code{\link{generatePartialPredictionData}} is called with the \code{interaction} argument \code{FALSE}
#'   (the default) with argument \code{features} of length greater than one, then \code{interaction} is ignored and
#'   the feature displayed is controlled by an interactive side panel.
#'   Default is \code{NULL}.
#' @return A ggvis object.
#' @export
plotPartialPredictionGGVIS = function(obj, interaction = NULL) {
  assertClass(obj, "PartialPredictionData")

  if (obj$interaction & length(obj$features) > 2L)
    stop("It is only possible to plot 2 features with this function.")

  bounds = all(c("lower", "upper") %in% colnames(obj$data) & obj$task.desc$type %in% c("surv", "regr"))

  if (!is.null(interaction) & !is.null(obj$interaction) & length(obj$features) == 2L) {
    if (!(interaction %in% obj$features))
      stop("interaction argument not found in features.")
    feature = obj$features[which(obj$features != interaction)]
  } else {
    feature = obj$features
    interaction = NULL
  }

  target = obj$target
  if (all(target %in% obj$task.desc$class.levels)) {
    data = reshape2::melt(obj$data, id.vars = obj$features, variable = "Class", value.name = "Probability")
    data$Class = gsub("^prob\\.", "", data$Class)
    if (length(unique(data$Class)) == 2L) {
      data = data[data$Class == obj$task.desc$positive, ]
      target = obj$task.desc$positive
    }
  } else {
    data = obj$data
  }

  create_plot = function(obj, data, target, feature, bounds) {
    if (all(target %in% obj$task.desc$class.levels)) {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name(feature)),
                         ggvis::prop("y", as.name("Probability")),
                         ggvis::prop("stroke", as.name("Class")))
    } else {
      plt = ggvis::ggvis(data, ggvis::prop("x", as.name(feature)),
                         ggvis::prop("y", as.name(target)))
    }
    plt = ggvis::layer_points(plt)
    plt = ggvis::layer_lines(plt)

    if (bounds)
      plt = ggvis::layer_ribbons(plt, ggvis::prop("y", as.name("lower")),
                                 ggvis::prop("y2", as.name("upper")),
                                 ggvis::prop("opacity", .5))
    plt = ggvis::add_axis(plt, "x", title = feature)
    if (length(obj$task.desc$type) == "response")
      plt = ggvis::add_axis(plt, "y", title = target)
    else
      plt = ggvis::add_axis(plt, "y", title = "Probability")
    plt
  }

  if ((!is.null(interaction) & length(obj$features) == 2L & obj$interaction) |
        (is.null(interaction) & length(obj$features) > 1L & !obj$interaction)) {
    if (!is.null(interaction) & length(obj$features == 2L))
      panel = shiny::selectInput("interaction_select", paste("choose a value of", interaction),
                                 unique(obj$data[[interaction]]))
    else
      panel = shiny::selectInput("interaction_select", "choose a feature", obj$features)

    ui = shiny::shinyUI(
      shiny::pageWithSidebar(
        shiny::headerPanel("Partial Prediction"),
        shiny::sidebarPanel(panel),
        shiny::mainPanel(
          shiny::uiOutput("ggvis_ui"),
          ggvis::ggvisOutput("ggvis")
        )
      ))
    server = shiny::shinyServer(function(input, output) {
      if (!is.null(interaction) & length(obj$features == 2L))
        plt = shiny::reactive(create_plot(obj, data[data[[interaction]] == input$interaction_select, ],
                                          target, feature, bounds))
      else
        plt = shiny::reactive(create_plot(obj, data[!is.na(data[[input$interaction_select]]), ],
                                          target, input$interaction_select, bounds))

      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    create_plot(obj, data, target, feature, bounds)
  }
}

doPartialPredictionIteration = function(obj, data, rng, features, fun, td, i, ...) {
  data[features] = rng[i, ]
  pred = do.call("predict", c(list("object" = obj, "newdata" = data), list(...)))$data
  if (obj$learner$predict.type == "response") {
    fun(pred$response)
  } else {
    cols = lapply(td$class.levels, function(x) grepl(x, colnames(pred)))
    cols = apply(do.call("rbind", cols), 2, any)
    apply(pred[, cols], 2, fun)
  }
}

generateFeatureGrid = function(feature, data, resample, fmin, fmax, cutoff) {
  if (is.factor(data[[feature]])) {
    factor(rep(levels(data[[feature]]), length.out = cutoff),
           levels = levels(data[[feature]]), ordered = is.ordered(data[[feature]]))
  } else {
    if (resample != "none") {
      sample(data[[feature]], cutoff, resample == "bootstrap")
    } else {
      if (is.integer(data[[feature]]))
        sort(rep(fmin:fmax, length.out = cutoff))
      else
        seq(fmin, fmax, length.out = cutoff)
    }
  }
}
