#' @title Generate partial predictions.
#'
#' @description
#' Estimate how the learned prediction function is affected by one or more features.
#' For a learned function f(x) where x is partitioned into x_s and x_c, the partial dependence of
#' f on x_s can be summarized by averaging over x_c and setting x_s to a range of values of interest,
#' estimating E_(x_c)(f(x_s, x_c)). The conditional expectation of f at observation i is estimated similarly.
#' Additionally, partial derivatives of the marginalized function w.r.t. the features can be computed.
#'
#' @family partial_prediction
#' @family generate_plot_data
#' @aliases PartialPredictionData
#'
#' @param obj [\code{\link{WrappedModel}}]\cr
#'   Result of \code{\link{train}}.
#' @param input [\code{data.frame} | \code{\link{Task}}]\cr
#'   Input data.
#' @param features [\code{character}]\cr
#'   A vector of feature names contained in the training data.
#'   If not specified all features in the \code{input} will be used.
#' @param interaction [\code{logical(1)}]\cr
#'   Whether the \code{features} should be interacted or not. If \code{TRUE} then the Cartesian product of the
#'   prediction grid for each feature is taken, and the partial prediction at each unique combination of
#'   values of the features is estimated. Note that if the length of \code{features} is greater than two,
#'   \code{\link{plotPartialPrediction}} and \code{\link{plotPartialPredictionGGVIS}} cannot be used.
#'   If \code{FALSE} each feature is considered separately. In this case \code{features} can be much longer
#'   than two.
#'   Default is \code{FALSE}.
#' @param derivative [\code{logical(1)}]\cr
#'   Whether or not the partial derivative of the learned function with respect to the features should be
#'   estimated. If \code{TRUE} \code{interaction} must be \code{FALSE}. The partial derivative of individual
#'   observations may be estimated. Note that computation time increases as the learned prediction function
#'   is evaluated at \code{gridsize} points * the number of points required to estimate the partial derivative.
#'   Additional arguments may be passed to \code{\link[numDeriv]{grad}} (for regression or survival tasks) or
#'   \code{\link[numDeriv]{jacobian}} (for classification tasks). Note that functions which are not smooth may
#'   result in estimated derivatives of 0 (for points where the function does not change within +/- epsilon)
#'   or estimates trending towards +/- infinity (at discontinuities).
#'   Default is \code{FALSE}.
#' @param individual [\code{logical(1)}]\cr
#'   Whether to plot the individual conditional expectation curves rather than the aggregated curve, i.e.,
#'   rather than aggregating (using \code{fun}) the partial predictions of \code{features}, plot the
#'   partial predictions of all observations in \code{data} across all values of the \code{features}.
#'   The algorithm is developed in Goldstein, Kapelner, Bleich, and Pitkin (2015).
#'   Default is \code{FALSE}.
#' @param center [\code{list}]\cr
#'   A named list containing the fixed values of the \code{features}
#'   used to calculate an individual partial prediction which is then
#'   subtracted from each individual partial prediction made across the prediction grid created for the
#'   \code{features}: centering the individual partial prediction lines to make them more interpretable.
#'   This argument is ignored if \code{individual != TRUE}.
#'   Default is \code{NULL}.
#' @param fun [\code{function}]\cr
#'   For regression, a function that accepts a numeric vector and returns either a single number
#'   such as a measure of location such as the mean, or three numbers, which give a lower bound,
#'   a measure of location, and an upper bound. Note if three numbers are returned they must be
#'   in this order. For classification with \code{predict.type = "prob"} the function must accept
#'   a numeric matrix with the number of columns equal to the number of class levels of the target.
#'   For classification with \code{predict.type = "response"} (the default) the function must accept
#'   a character vector and output a numeric vector with length equal to the number of classes in the
#'   target feature.
#'   The default is the mean, unless \code{obj} is classification with \code{predict.type = "response"}
#'   in which case the default is the proportion of observations predicted to be in each class.
#' @param bounds [\code{numeric(2)}]\cr
#'   The value (lower, upper) the estimated standard error is multiplied by to estimate the bound on a
#'   confidence region for a partial prediction. Ignored if \code{predict.type != "se"} for the learner.
#'   Default is the 2.5 and 97.5 quantiles (-1.96, 1.96) of the Gaussian distribution.
#' @param resample [\code{character(1)}]\cr
#'   Defines how the prediction grid for each feature is created. If \dQuote{bootstrap} then
#'   values are sampled with replacement from the training data. If \dQuote{subsample} then
#'   values are sampled without replacement from the training data. If \dQuote{none} an evenly spaced
#'   grid between either the empirical minimum and maximum, or the minimum and maximum defined by
#'   \code{fmin} and \code{fmax}, is created.
#'   Default is \dQuote{none}.
#' @param fmin [\code{numeric}]\cr
#'   The minimum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = NULL} and when the empirical minimum is higher
#'   than the theoretical minimum for a given feature. This only applies to numeric features and a
#'   \code{NA} should be inserted into the vector if the corresponding feature is a factor.
#'   Default is the empirical minimum of each numeric feature and NA for factor features.
#' @param fmax [\code{numeric}]\cr
#'   The maximum value that each element of \code{features} can take.
#'   This argument is only applicable if \code{resample = "none"} and when the empirical maximum is lower
#'   than the theoretical maximum for a given feature. This only applies to numeric features and a
#'   \code{NA} should be inserted into the vector if the corresponding feature is a factor.
#'   Default is the empirical maximum of each numeric feature and NA for factor features.
#' @param gridsize [\code{integer(1)}]\cr
#'   The length of the prediction grid created for each feature.
#'   If \code{resample = "bootstrap"} or \code{resample = "subsample"} then this defines
#'   the number of (possibly non-unique) values resampled. If \code{resample = NULL} it defines the
#'   length of the evenly spaced grid created.
#' @param ... additional arguments to be passed to \code{\link{predict}}.
#' @return [\code{PartialPredictionData}]. A named list, which contains the partial predictions,
#'   input data, target, features, task description, and other arguments controlling the type of
#'   partial predictions made.
#'
#' Object members:
#'   \item{data}{[\code{data.frame}]\cr
#'     Has columns for the prediction: one column for regression and
#'     survival analysis, and a column for class and the predicted probability for classification as well
#'     as a a column for each element of \code{features}. If \code{individual = TRUE} then there is an
#'     additional column \code{idx} which gives the index of the \code{data} that each prediction corresponds to.}
#'   \item{task.desc}{[\code{\link{TaskDesc}}]\cr
#'     Task description.}
#'   \item{target}{Target feature for regression, target feature levels for classification,
#'         survival and event indicator for survival.}
#'   \item{features}{[\code{character}]\cr
#'     Features argument input.}
#'   \item{interaction}{[\code{logical(1)}]\cr
#'     Whether or not the features were interacted (i.e. conditioning).}
#'   \item{derivative}{[\code{logical(1)}]\cr
#'     Whether or not the partial derivative was estimated.}
#'   \item{individual}{[\code{logical(1)}]\cr
#'     Whether the partial predictions were aggregated or the individual curves are retained.}
#'   \item{center}{[\code{logical(1)}]\cr
#'     If \code{individual == TRUE} whether the partial prediction at the values of the
#'                 features specified was subtracted from the individual partial predictions. Only displayed if
#'                 \code{individual == TRUE}.}
#' @references
#' Goldstein, Alex, Adam Kapelner, Justin Bleich, and Emil Pitkin. \dQuote{Peeking inside the black box: Visualizing statistical learning with plots of individual conditional expectation.} Journal of Computational and Graphical Statistics. Vol. 24, No. 1 (2015): 44-65.
#'
#' Friedman, Jerome. \dQuote{Greedy Function Approximation: A Gradient Boosting Machine.} The Annals of Statistics. Vol. 29. No. 5 (2001): 1189-1232.
#' @examples
#' lrn = makeLearner("regr.rpart")
#' fit = train(lrn, bh.task)
#' pd = generatePartialPredictionData(fit, bh.task, "lstat")
#' plotPartialPrediction(pd)
#'
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, iris.task)
#' pd = generatePartialPredictionData(fit, iris.task, "Petal.Width")
#' plotPartialPrediction(pd)
#' @export
generatePartialPredictionData = function(obj, input, features,
                                         interaction = FALSE, derivative = FALSE,
                                         individual = FALSE, center = NULL,
                                         fun = mean, bounds = c(qnorm(.025), qnorm(.975)),
                                         resample = "none",
                                         fmin, fmax, gridsize = 10L, ...) {
  assertClass(obj, "WrappedModel")
  if (obj$learner$predict.type == "se" & individual)
    stop("individual = TRUE not compatabile with predict.type = 'se'!")
  if (obj$learner$predict.type == "se" & derivative)
    stop("derivative = TRUE is not compatible with predict.type = 'se'!")
  if (!inherits(input, c("Task", "data.frame")))
    stop("input must be a Task or a data.frame!")
  if (inherits(input, "Task")) {
    data = getTaskData(input)
    td = input$task.desc
  } else {
    data = input
    td = obj$task.desc
    assertDataFrame(data, col.names = "unique", min.rows = 1L, min.cols = length(obj$features) + length(td$target))
    assertSetEqual(colnames(data), c(obj$features, td$target), ordered = FALSE)
  }

  if (missing(features))
    features = colnames(data)[!colnames(data) %in% td$target]
  else
    assertSubset(features, obj$features)

  assertFlag(interaction)
  assertFlag(derivative)
  if (derivative & interaction)
    stop("interaction cannot be TRUE if derivative is TRUE.")
  if (derivative) {
    if (any(sapply(data[, features, drop = FALSE], class) %in% c("factor", "ordered", "character")))
      stop("All features must be numeric to estimate set derivative = TRUE!")
  }
  assertFlag(individual)
  if (individual)
    fun = function(x) x
  if (!is.null(center)) {
    if (derivative)
      stop("center cannot be used with derivative = TRUE.")
    assertList(center, len = length(features), names = "unique")
    if (!all(names(center) %in% features))
      stop("The names of the elements in center must be the same as the features.")
    center = as.data.frame(do.call("cbind", center))
  }
  assertFunction(fun)
  assertNumeric(bounds, len = 2L)
  assertNumber(bounds[1], upper = 0)
  assertNumber(bounds[2], lower = 0)
  assertChoice(resample, c("none", "bootstrap", "subsample"))

  if (missing(fmin))
    fmin = lapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
                                               min(data[[x]], na.rm = TRUE), NA))
  if (missing(fmax))
    fmax = lapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
                                               max(data[[x]], na.rm = TRUE), NA))
  assertList(fmin, len = length(features))
  if (!all(names(fmin) %in% features))
    stop("fmin must be a named list with an NA or value corresponding to each feature.")
  assertList(fmax, len = length(features))
  if (!all(names(fmax) %in% features))
    stop("fmax must be a named list with an NA or value corresponding to each feature.")
  assertCount(gridsize, positive = TRUE)

  rng = vector("list", length(features))
  names(rng) = features
  for (i in 1:length(features))
    rng[[i]] = generateFeatureGrid(features[i], data, resample, fmax[[i]], fmin[[i]], gridsize)
  if (length(features) > 1L & interaction)
    rng = expand.grid(rng)

  if (!individual) {
    ## check that aggregation function returns input of valid length and type
    ## if individual then function is not applied
    test = fun(1:3)
    if (!is.numeric(test))
      stop("fun argument must return a numeric vector")
    if (td$type == "classif" & obj$learner$predict.type == "response" & length(test) != 3L)
      stop("function argument must return a numeric vector with length equal to the number of target class levels.")
    if (td$type == "classif" & obj$learner$predict.type == "prob" & length(test) != 1L)
      stop("function argument must return a numeric vector of length 1.")
    if (td$type == "regr" & !(length(test) %in% c(1L, 3L)))
      stop("function argument must return a numeric vector of length 1 or 3.")
  }

  if (td$type == "regr")
    target = td$target
  else if (td$type == "classif") {
    if (length(td$class.levels) > 2L)
      target = td$class.levels
    else
      target = td$positive
  }  else
    target = "Risk"

  if (length(features) > 1L & !interaction) {
    out = lapply(features, function(x) {
      if (derivative) {
        args = list(obj = obj, data = data, features = x, fun = fun, td = td, individual = individual,
                    bounds = bounds, ...)
        out = parallelMap(doPartialDerivativeIteration, x = rng[[x]], more.args = args)
        rng = as.data.frame(rng[[x]])
        colnames(rng) = x
        centerpred = NULL
      } else {
        rng = as.data.frame(rng[[x]])
        colnames(rng) = x
        args = list(obj = obj, data = data, fun = fun, td = td, rng = rng, features = x, bounds = bounds, ...)
        out = parallelMap(doPartialPredictionIteration, i = seq_len(nrow(rng)), more.args = args)
        if (!is.null(center) & individual)
          centerpred = doPartialPredictionIteration(obj, data, center[, x, drop = FALSE],
                                                    x, fun, td, 1, bounds = bounds)
        else
          centerpred = NULL
      }
      if (!individual)
        doAggregatePartialPrediction(out, td, target, x, test, rng)
      else
        doIndividualPartialPrediction(out, td, nrow(data), rng, target, x, centerpred)
    })
    out = plyr::ldply(out)
  } else {
    if (derivative) {
      args = list(obj = obj, data = data, features = features, fun = fun, td = td, individual = individual, ...)
      out = parallelMap::parallelMap(doPartialDerivativeIteration, x = rng[[features]], more.args = args)
      centerpred = NULL
      rng = as.data.frame(rng)
      colnames(rng) = features
    } else {
      rng = as.data.frame(rng)
      colnames(rng) = features
      args = list(obj = obj, data = data, fun = fun, td = td, rng = rng, features = features, bounds = bounds, ...)
      out = parallelMap::parallelMap(doPartialPredictionIteration, i = seq_len(nrow(rng)), more.args = args)
      if (!is.null(center) & individual)
        centerpred = as.data.frame(doPartialPredictionIteration(obj, data, center, features, fun, td, 1, bounds))
      else
        centerpred = NULL
    }
    if (!individual)
      out = doAggregatePartialPrediction(out, td, target, features, test, rng)
    else
      out = doIndividualPartialPrediction(out, td, nrow(data), rng, target, features, centerpred)
  }

  if (td$type %in% c("regr", "surv"))
    out = out[, c(target, features, colnames(out)[!colnames(out) %in% c(target, features)])]
  else
    out = out[, c("Class", "Probability", features,
                  colnames(out)[!colnames(out) %in% c("Class", "Probability", features)])]

  makeS3Obj("PartialPredictionData",
            data = out,
            task.desc = td,
            target = target,
            features = features,
            interaction = interaction,
            derivative = derivative,
            individual = individual,
            center = !is.null(center))
}

doPartialDerivativeIteration = function(x, obj, data, features, fun, td, individual, ...) {
  if (!individual) {
    ## construct function appropriate for numDeriv w/ aggregate predictions
    f = function(x, obj, data, features, fun, td, ...) {
      data[features] = x
      pred = do.call("predict", c(list("object" = obj, "newdata" = data), list(...)))
      if (obj$learner$predict.type == "response")
        fun(getPredictionResponse(pred))
      else if (length(obj$task.desc$class.levels) == 2L)
        fun(getPredictionProbabilities(pred))
      else
        apply(getPredictionProbabilities(pred), 2, fun)
    }
    if (obj$learner$predict.type == "response")
      numDeriv::grad(func = f, x = x, obj = obj, data = data, features = features, fun = fun, td = td)
    else
      t(numDeriv::jacobian(func = f, x = x, obj = obj, data = data, features = features, fun = fun, td = td, ...))
  } else {
    f = function(x, obj, data, features, fun, td, ...) {
      data[features] = x
      pred = do.call("predict", c(list("object" = obj, "newdata" = data), list(...)))
      if (obj$learner$predict.type == "response")
        getPredictionResponse(pred)
      else
        as.numeric(getPredictionProbabilities(pred))
    }
    if (obj$learner$predict.type == "response")
      sapply(1:nrow(data), function(idx)
        numDeriv::grad(func = f, x = x, obj = obj, data = data[idx,, drop = FALSE], features = features,
                       fun = fun, td = td))
    else
      t(sapply(1:nrow(data), function(idx) numDeriv::jacobian(func = f, x = x, obj = obj,
                                                              data = data[idx,, drop = FALSE],
                                                              features = features, fun = fun, td = td, ...)))
  }
}

doPartialPredictionIteration = function(obj, data, rng, features, fun, td, i, bounds, ...) {
  data[features] = rng[i, ]
  pred = do.call("predict", c(list("object" = obj, "newdata" = data), list(...)))
  if (obj$learner$predict.type == "response")
    fun(getPredictionResponse(pred))
  else if (length(obj$task.desc$class.levels) == 2L)
    fun(getPredictionProbabilities(pred))
  else if (obj$learner$predict.type == "se") {
    point = getPredictionResponse(pred)
    out = cbind(point + outer(getPredictionSE(pred), bounds), point)[, c(1, 3, 2)]
    unname(apply(out, 2, fun))
  } else
    apply(getPredictionProbabilities(pred), 2, fun)
}

generateFeatureGrid = function(feature, data, resample, fmin, fmax, gridsize) {
  nunique = ifelse(length(feature) > 1L, nrow(unique(data[feature, ])), length(unique(data[[feature]])))
  cutoff = ifelse(gridsize >= nunique, nunique, gridsize)

  if (is.factor(data[[feature]])) {
    factor(rep(levels(data[[feature]]), length.out = cutoff),
           levels = levels(data[[feature]]), ordered = is.ordered(data[[feature]]))
  } else {
    if (resample != "none") {
      sort(sample(data[[feature]], cutoff, resample == "bootstrap"))
    } else {
      if (is.integer(data[[feature]]))
        sort(rep(fmin:fmax, length.out = cutoff))
      else
        seq(fmin, fmax, length.out = cutoff)
    }
  }
}

doAggregatePartialPrediction = function(out, td, target, features, test, rng) {
  out = as.data.frame(do.call("rbind", out))
  if (td$type == "regr" & ncol(out) == 3L)
    colnames(out) = c("lower", target, "upper")
  else
    colnames(out) = target
  out = cbind(out, rng)
  if (td$type == "regr" & all(c("upper", "lower", target) %in% colnames(out)))
    if (!all(out$lower <= out[[target]] & out[[target]] <= out$upper))
      stop("function argument must return a sorted numeric vector ordered lowest to highest.")

  if (all(target %in% td$class.levels)) {
    out = reshape2::melt(out, id.vars = features, variable = "Class", value.name = "Probability")
    out$Class = gsub("^prob\\.", "", out$Class)
  }
  out
}

doIndividualPartialPrediction = function(out, td, n = nrow(data), rng, target, features, centerpred = NULL) {
  if (td$type == "classif" & length(td$class.levels) > 2L) {
    if (!is.null(centerpred))
      out = lapply(out, function(x) x - centerpred)
    out = as.data.frame(do.call("rbind", out))
    colnames(out) = target
    idx = rep(seq_len(n), nrow(rng))
    rng = rng[rep(seq_len(nrow(rng)), each = n), , drop = FALSE]
    out = cbind(out, rng, idx, row.names = NULL)
    out = reshape2::melt(out, id.vars = c(features, "idx"),
                         variable.name = "Class", value.name = "Probability")
    out$idx = interaction(out$idx, out$Class)
  } else {
    out = as.data.frame(do.call("rbind", out))
    if (!is.null(centerpred))
      out = t(apply(out, 1, function(x) x - centerpred))
    colnames(out) = 1:n
    out = cbind(out, rng)
    out = reshape2::melt(out, id.vars = features, variable.name = "idx", value.name = target)
    if (td$type == "classif")
      out = reshape2::melt(out, id.vars = c(features, "idx"), value.name = "Probability", variable.name = "Class")
  }
  out
}
#' @export
print.PartialPredictionData = function(x, ...) {
  catf("PartialPredictionData")
  catf("Task: %s", x$task.desc$id)
  catf("Features: %s", paste(x$features, collapse = ", "))
  catf("Target: %s", paste(x$target, collapse = ", "))
  catf("Derivative: %s", x$derivative)
  catf("Interaction: %s", x$interaction)
  catf("Individual: %s", x$individual)
  if (x$individual)
    catf("Predictions centered: %s", x$center)
  print(head(x$data))
}
#' @title Plot a partial prediction with ggplot2.
#' @description
#' Plot a partial prediction from \code{\link{generatePartialPredictionData}} using ggplot2.
#'
#' @family partial_prediction
#' @family plot
#'
#' @param obj [\code{PartialPredictionData}]\cr
#'   Generated by \code{\link{generatePartialPredictionData}}.
#' @param geom [\code{charater(1)}]\cr
#'   The type of geom to use to display the data. Can be \dQuote{line} or \dQuote{tile}.
#'   For tiling at least two features must be used with \code{interaction = TRUE} in the call to
#'   \code{\link{generatePartialPredictionData}}. This may be used in conjuction with the
#'   \code{facet} argument if three features are specified in the call to
#'   \code{\link{generatePartialPredictionData}}.
#'   Default is \dQuote{line}.
#' @param facet [\code{character(1)}]\cr
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
#' @param p [\code{numeric(1)}]\cr
#'   If \code{individual = TRUE} then \code{sample} allows the user to sample without replacement
#'   from the output to make the display more readable. Each row is sampled with probability \code{p}.
#'   Default is \code{1}.
#' @template ret_gg2
#' @export
plotPartialPrediction = function(obj, geom = "line", facet = NULL, p = 1) {
  assertClass(obj, "PartialPredictionData")
  if (length(obj$features) > 2L & geom != "tile" & obj$interaction)
    stop("To plot more than 2 features geom must be 'tile'!")
  assertChoice(geom, c("tile", "line"))
  if (geom == "tile") {
    if (!(obj$task.desc$type %in% c("regr", "surv"))) {
      if (length(obj$task.desc$class.levels) > 2L)
        stop("Only visualization of binary classification works with tiling!")
    }

    feat_classes = sapply(obj$data, class)
    if (any(feat_classes == "factor")) {
      fact_feats = names(feat_classes[feat_classes == "factor"])
      if (!is.null(facet))
        fact_feats = fact_feats[which(fact_feats != facet)]
      do_not_contour = length(fact_feats) > 0L
    } else
      do_not_contour = FALSE

    if (do_not_contour)
      warning("Factor features cannot be used to create contour plots! only tiles will be displayed.")
    if (!obj$interaction)
      stop("generatePartialPredictionData was called with interaction = FALSE!")
  }

  if (!is.null(facet)) {
    assertChoice(facet, obj$features)
    if (!length(obj$features) %in% 2:3)
      stop("generatePartialPrediction must be called with two or three features to use this argument!")
    if (!obj$interaction)
      stop("generatePartialPrediction must be called with interaction = TRUE to use this argument!")
    features = obj$features[which(obj$features != facet)]
    if (!is.factor(obj$data[[facet]]))
      obj$data[[facet]] = paste(facet, "=", as.factor(signif(obj$data[[facet]], 2)), sep = " ")
    else
      obj$data[[facet]] = paste(facet, "=", obj$data[[facet]])
    scales = "fixed"
  } else {
    features = obj$features
    if (length(features) > 1L & !(length(features) == 2L & geom == "tile")) {
      facet = "Feature"
      scales = "free_x"
    }
  }

  if (p != 1) {
    assertNumber(p, lower = 0, upper = 1, finite = TRUE)
    if (!obj$individual)
      stop("generatePartialPredictionData must be called with individual = TRUE to use this argument!")
    rows = unique(obj$data$idx)
    id = sample(rows, size = floor(p * length(rows)))
    obj$data = obj$data[which(obj$data$idx %in% id), ]
  }

  if (obj$task.desc$type %in% c("regr", "classif"))
    target = obj$task.desc$target
  else
    target = "Risk"

  bounds = all(c("lower", "upper") %in% colnames(obj$data) & obj$task.desc$type %in% c("surv", "regr") &
                 length(features) < 3L & geom == "line")

  if (geom == "line") {
    obj$data = melt(obj$data, id.vars = colnames(obj$data)[!colnames(obj$data) %in% features],
                    variable = "Feature", value.name = "Value", na.rm = TRUE)
    if (!obj$individual) {
      if (obj$task.desc$type %in% c("regr", "surv"))
        plt = ggplot(obj$data, aes_string("Value", target))
      else
        plt = ggplot(obj$data, aes_string("Value", "Probability", group = "Class", color = "Class"))
    } else {
      if (obj$task.desc$type %in% c("regr", "surv"))
        plt = ggplot(obj$data, aes_string("Value", target, group = "idx"))
      else
        plt = ggplot(obj$data, aes_string("Value", "Probability", group = "idx", color = "Class"))
    }

    if (length(features) == 1L) {
      if (obj$task.desc$type %in% c("regr", "surv"))
        plt = plt + labs(x = features, y = target)
      else
        plt = plt + labs(x = features)
    }

    if (obj$individual)
      plt = plt + geom_line(alpha = .25)
    else
      plt = plt + geom_line() + geom_point()

    if (bounds)
      plt = plt + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = .5)

    if (obj$center)
      plt = plt + ylab(paste(target, "(centered)"))

    if (obj$derivative)
      plt = plt + ylab(paste(target, "(derivative)"))
  } else { ## tiling
    plt = ggplot(obj$data, aes_string(x = features[1], y = features[2], z = target))
    plt = plt + geom_tile(aes_string(fill = target))
    if (!do_not_contour)
      plt = plt + stat_contour()
  }

  if (!is.null(facet))
    plt = plt + facet_wrap(as.formula(paste0("~ ", facet)), scales = scales)

  plt
}
#' @title Plot a partial prediction using ggvis.
#' @description
#' Plot a partial prediction from \code{\link{generatePartialPredictionData}} using ggvis.
#'
#' @family partial_prediction
#' @family plot
#'
#' @param obj [\code{PartialPredictionData}]\cr
#'   Generated by \code{\link{generatePartialPredictionData}}.
#' @param interact [\code{character(1)}]\cr
#'   The name of a feature to be mapped to an interactive sidebar using Shiny.
#'   This feature must have been an element of the \code{features} argument to
#'   \code{\link{generatePartialPredictionData}} and is only applicable when said argument had length
#'   greater than 1.
#'   If \code{\link{generatePartialPredictionData}} is called with the \code{interaction} argument \code{FALSE}
#'   (the default) with argument \code{features} of length greater than one, then \code{interact} is ignored and
#'   the feature displayed is controlled by an interactive side panel.
#'   Default is \code{NULL}.
#' @param p [\code{numeric(1)}]\cr
#'   If \code{individual = TRUE} then \code{sample} allows the user to sample without replacement
#'   from the output to make the display more readable. Each row is sampled with probability \code{p}.
#'   Default is \code{1}.
#' @template ret_ggv
#' @export
plotPartialPredictionGGVIS = function(obj, interact = NULL, p = 1) {
  assertClass(obj, "PartialPredictionData")
  if (!is.null(interact))
    assertChoice(interact, obj$features)
  if (obj$interaction & length(obj$features) > 2L)
    stop("It is only possible to plot 2 features with this function.")

  if (p != 1) {
    assertNumber(p, lower = 0, upper = 1, finite = TRUE)
    if (!obj$individual)
      stop("generatePartialPredictionData must be called with individual = TRUE to use this argument!")
    rows = unique(obj$data$idx)
    id = sample(rows, size = floor(p * length(rows)))
    obj$data = obj$data[which(obj$data$idx %in% id), ]
  }

  if (obj$interaction & length(obj$features) == 2L) {
    if (is.null(interact))
      interact = obj$features[-which.max(sapply(obj$features, function(x) length(unique(obj$data[[x]]))))]
    x = obj$features[which(obj$features != interact)]
    if (is.factor(obj$data[[interact]]))
      choices = levels(obj$data[[interact]])
    else
      choices = sort(unique(obj$data[[interact]]))
  } else if (!obj$interaction & length(obj$features) > 1L) {
    id = colnames(obj$data)[!colnames(obj$data) %in% obj$features]
    obj$data = reshape2::melt(obj$data, id.vars = id, variable.name = "Feature",
                              value.name = "Value", na.rm = TRUE)
    interact = "Feature"
    choices = obj$features
  } else
    interact = NULL

  bounds = all(c("lower", "upper") %in% colnames(obj$data) & obj$task.desc$type %in% c("surv", "regr"))

  if (obj$task.desc$type %in% c("regr", "classif"))
    target = obj$task.desc$target
  else
    target = "Risk"

  create_plot = function(td, target, interaction, individual, data, x, bounds) {
    classif = td$type == "classif" & all(target %in% td$class.levels)
    if (classif) {
      if (interaction)
        plt = ggvis::ggvis(data, ggvis::prop("x", as.name(x)),
                           ggvis::prop("y", as.name("Probability")),
                           ggvis::prop("stroke", as.name("Class")))
      else ## no interaction but multiple features
        plt = ggvis::ggvis(data, ggvis::prop("x", as.name("Value")),
                           ggvis::prop("y", as.name("Probability")),
                           ggvis::prop("stroke", as.name("Class")))
    } else { ## regression/survival
      if (interaction)
        plt = ggvis::ggvis(data, ggvis::prop("x", as.name(x)),
                           ggvis::prop("y", as.name(target)))
      else
        plt = ggvis::ggvis(data, ggvis::prop("x", as.name("Value")),
                           ggvis::prop("y", as.name(target)))
    }

    if (bounds)
      plt = ggvis::layer_ribbons(plt, ggvis::prop("y", as.name("lower")),
                                 ggvis::prop("y2", as.name("upper")),
                                 ggvis::prop("opacity", .5))
    if (individual) {
      plt = ggvis::group_by_.ggvis(plt, as.name("idx"))
      plt = ggvis::layer_paths(plt, ggvis::prop("opacity", .25))
    } else {
      if (classif)
        plt = ggvis::layer_points(plt, ggvis::prop("fill", as.name("Class")))
      else
        plt = ggvis::layer_points(plt)
      plt = ggvis::layer_lines(plt)
    }

    plt
  }

  if (obj$center)
    header = paste(target, "(centered)")
  else if (obj$derivative)
    header = paste(target, "(derivative)")
  else
    header = target

  if (!is.null(interact)) {
    panel = shiny::selectInput("interaction_select", interact, choices)
    ui = shiny::shinyUI(
      shiny::pageWithSidebar(
        shiny::headerPanel(header),
        shiny::sidebarPanel(panel),
        shiny::mainPanel(
          shiny::uiOutput("ggvis_ui"),
          ggvis::ggvisOutput("ggvis")
        )
      ))
    server = shiny::shinyServer(function(input, output) {
      plt = shiny::reactive(create_plot(obj$task.desc, obj$target, obj$interaction, obj$individual,
                                        obj$data[obj$data[[interact]] == input$interaction_select, ],
                                        x, bounds))
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
    })
    shiny::shinyApp(ui, server)
  } else
    create_plot(obj$task.desc, obj$target, obj$interaction, obj$individual, obj$data, obj$features, bounds)
}
