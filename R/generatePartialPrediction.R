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
#'   values are sampled without replacement from the training data. If \code{NULL} an evenly spaced
#'   grid between either the empirical minimum and maximum, or the minimum and maximum defined by
#'   \code{fmin} and \code{fmax}, is created.
#'   Default is \code{NULL}.
#' @param fmin \code{numeric}\cr
#'   The minimum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = NULL} and when the empirical minimum is higher
#'   than the theoretical minimum for a given feature.
#'   Default is \code{NULL}.
#' @param fmax \code{numeric}\cr
#'   The maximum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = NULL} and when the empirical maximum is lower
#'   than the theoretical maximum for a given feature.
#'   Default is \code{NULL}.
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
#' pd = generatePartialPredictionData(fit, getTaskData(iris.task), "Petal.Width")
#' plotPartialPrediction(pd)
#' @export
generatePartialPredictionData = function(obj, data, features, fun = mean,
                                         resample = NULL, fmin = NULL, fmax = NULL,
                                         gridsize = 10L, ...) {
  checkmate::assertClass(obj, "WrappedModel")
  td = obj$task.desc
  rng = lapply(features, function(x) generateFeatureGrid(x, data, resample, fmin, fmax, gridsize))
  rng = as.data.frame(rng)
  if (length(features) > 1L)
    rng = expand.grid(rng)

  args = list(obj = obj, data = data, fun = fun, td = td, rng = rng, features = features, ...)
  ppred = parallelMap::parallelMap(doPartialPredictionIteration, seq_len(nrow(rng)), more.args = args)
  ppred = as.data.frame(do.call("rbind", ppred))

  if (td$type %in% c("regr", "surv")) {
    target = td$target
  } else {
    checkmate::assert(ncol(ppred) == length(td$class.levels))
    target = td$class.levels
  }
  data = cbind(ppred, rng)
  if ((ncol(ppred) == 1L & td$type == "regr") | td$type == "classif")
    colnames(data) = c(target, features)
  else if (ncol(ppred) == 3L & td$type == "regr") {
    colnames(data) = c("lower", target, "upper", features)
    checkmate::assert(all(data$lower <= data[[target]] & data[[target]] <= data$upper))
  } else {
    checkmate::assert(td$type == "surv")
    checkmate::assert(ncol(ppred) == 1L)
    colnames(data) = c("risk", features)
  }
  makeS3Obj("PartialPredictionData",
            data = data,
            task.desc = td,
            target = target,
            features = features)
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
#'   \item{features}{Features argument input}.
#'   \item{target}{Target feature for regression, target feature levels for classification,
#'         survival and event indicator for survival.}
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
  print(head(x$data))
}
#' @title Plot a partial prediction
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
#'   Default is \code{NULL}.
#' @return A ggplot2 object.
#' @export
plotPartialPrediction = function(obj, facet = NULL) {
  checkmate::assertClass(obj, "PartialPredictionData")
  checkmate::assert(length(obj$features) <= 2L)
  bounds = all(c("lower", "upper") %in% colnames(obj$data))
  if (!is.null(facet)) {
    checkmate::assert(facet %in% obj$features & length(obj$features) > 1L)
    feature = obj$features[which(obj$features != facet)]
    if (!is.factor(obj$data[[facet]]))
      obj$data[[facet]] = paste(facet, "=", as.factor(signif(obj$data[[facet]], 2)), sep = " ")
    else
      obj$data[[facet]] = paste(facet, "=", obj$data[[facet]])
  } else {
    feature = obj$features
    facet = NULL
  }

  if (obj$task.desc$type == "surv")
    target = "risk"
  else
    target = obj$target

  if (all(target %in% obj$task.desc$class.levels)) {
    out = reshape2::melt(obj$data, id.vars = obj$features, variable = "Class", value.name = "Probability")
    out$Class = gsub("^prob\\.", "", out$Class)
    plt = ggplot2::ggplot(out, ggplot2::aes_string(feature, "Probability", color = "Class"))
  } else {
    plt = ggplot2::ggplot(obj$data, ggplot2::aes_string(feature, target))
  }
  plt = plt + ggplot2::geom_point() + ggplot2::geom_line()

  if (bounds)
    plt = plt + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "lower", ymax = "upper"), alpha = .5)

  if (!is.null(facet)) {
    plt = plt + ggplot2::facet_wrap(as.formula(paste("~", facet)))
  }

  plt
}
#' @title Plot a partial prediction
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
#'   Default is \code{NULL}.
#' @return A ggvis object.
#' @export
plotPartialPredictionGGVIS = function(obj, interaction = NULL) {
  checkmate::assertClass(obj, "PartialPredictionData")
  checkmate::assert(length(obj$features) <= 2L)
  bounds = all(c("lower", "upper") %in% colnames(obj$data))
  if (!is.null(interaction)) {
    checkmate::assert(interaction %in% obj$features & length(obj$features) > 1L)
    feature = obj$features[which(obj$features != interaction)]
  } else {
    feature = obj$features
    interaction = NULL
  }

  if (obj$task.desc$type == "surv")
    target = "risk"
  else
    target = obj$target

  if (all(target %in% obj$task.desc$class.levels)) {
    data = reshape2::melt(obj$data, id.vars = obj$features, variable = "Class", value.name = "Probability")
    data$Class = gsub("^prob\\.", "", data$Class)
  } else {
    data = obj$data
  }

  create_plot = function(obj, data, target, feature) {
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
    plt
  }

  if (!is.null(interaction) & length(obj$features) > 1L) {
    ui = shiny::shinyUI(
      shiny::pageWithSidebar(
        shiny::headerPanel("Partial Prediction"),
        shiny::sidebarPanel(
          shiny::selectInput("interaction_select",
                             paste("choose a value of", interaction),
                             unique(obj$data[[interaction]]))
        ),
        shiny::mainPanel(
          shiny::uiOutput("ggvis_ui"),
          ggvis::ggvisOutput("ggvis")
        )
      ))
    server = shiny::shinyServer(function(input, output) {
      data_sub = shiny::reactive(data[data[[interaction]] == input$interaction_select, ])
      plt = create_plot(obj, data_sub, target, feature)
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else {
    create_plot(obj, data, target, feature)
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

generateFeatureGrid = function(feature, data, resample = NULL,
                               fmin = NULL, fmax = NULL, cutoff = 10L) {
  if (is.factor(data[[feature]])) {
    factor(rep(levels(data[[feature]]), length.out = cutoff),
           levels = levels(data[[feature]]), ordered = is.ordered(data[[feature]]))
  } else {
    if (is.null(fmin))
      fmin = min(data[[feature]], na.rm = TRUE)
    if (is.null(fmax))
      fmax = max(data[[feature]], na.rm = TRUE)
    if (!is.null(resample)) {
      checkmate::assertChoice(resample, c("bootstrap", "subsample"))
      sample(data[[feature]], cutoff, resample == "bootstrap")
    } else
      seq(fmin, fmax, length.out = cutoff)
  }
}
