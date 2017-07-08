#' @title Generate partial dependence.
#'
#' @description
#' Estimate how the learned prediction function is affected by one or more features.
#' For a learned function f(x) where x is partitioned into x_s and x_c, the partial dependence of
#' f on x_s can be summarized by averaging over x_c and setting x_s to a range of values of interest,
#' estimating E_(x_c)(f(x_s, x_c)). The conditional expectation of f at observation i is estimated similarly.
#' Additionally, partial derivatives of the marginalized function w.r.t. the features can be computed.
#'
#' @family partial_dependence
#' @family generate_plot_data
#' @aliases PartialDependenceData
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
#'   prediction grid for each feature is taken, and the partial dependence at each unique combination of
#'   values of the features is estimated. Note that if the length of \code{features} is greater than two,
#'   \code{\link{plotPartialDependence}} and \code{\link{plotPartialDependenceGGVIS}} cannot be used.
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
#'   rather than aggregating (using \code{fun}) the partial dependences of \code{features}, plot the
#'   partial dependences of all observations in \code{data} across all values of the \code{features}.
#'   The algorithm is developed in Goldstein, Kapelner, Bleich, and Pitkin (2015).
#'   Default is \code{FALSE}.
#' @param center [\code{list}]\cr
#'   A named list containing the fixed values of the \code{features}
#'   used to calculate an individual partial dependence which is then
#'   subtracted from each individual partial dependence made across the prediction grid created for the
#'   \code{features}: centering the individual partial dependence lines to make them more interpretable.
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
#'   target feature. Two variables, \code{data} and \code{newdata} are made available to \code{fun} internally via a
#'   wrapper. `data` is the training data from `input` and `newdata` contains a single point from the
#'   prediction grid for \code{features} along with the training data for features not in \code{features}.
#'   This allows the computation of weights based on comparisons of the prediction grid to the training data.
#'   The default is the mean, unless \code{obj} is classification with \code{predict.type = "response"}
#'   in which case the default is the proportion of observations predicted to be in each class.
#' @param bounds [\code{numeric(2)}]\cr
#'   The value (lower, upper) the estimated standard error is multiplied by to estimate the bound on a
#'   confidence region for a partial dependence. Ignored if \code{predict.type != "se"} for the learner.
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
#' @param range [\code{list}]\cr
#'   The range of values of the feature you would want the partial plots on - passed as a numeric list
#' @param ... additional arguments to be passed to \code{\link{predict}}.
#' @return [\code{PartialDependenceData}]. A named list, which contains the partial dependence,
#'   input data, target, features, task description, and other arguments controlling the type of
#'   partial dependences made.
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
#'     Whether the partial dependences were aggregated or the individual curves are retained.}
#'   \item{center}{[\code{logical(1)}]\cr
#'     If \code{individual == TRUE} whether the partial dependence at the values of the
#'                 features specified was subtracted from the individual partial dependences. Only displayed if
#'                 \code{individual == TRUE}.}
#' @references
#' Goldstein, Alex, Adam Kapelner, Justin Bleich, and Emil Pitkin. \dQuote{Peeking inside the black box: Visualizing statistical learning with plots of individual conditional expectation.} Journal of Computational and Graphical Statistics. Vol. 24, No. 1 (2015): 44-65.
#'
#' Friedman, Jerome. \dQuote{Greedy Function Approximation: A Gradient Boosting Machine.} The Annals of Statistics. Vol. 29. No. 5 (2001): 1189-1232.
#' @examples
#' lrn = makeLearner("regr.svm")
#' fit = train(lrn, bh.task)
#' pd = generatePartialDependenceData(fit, bh.task, "lstat")
#' plotPartialDependence(pd, data = getTaskData(bh.task))
#'
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, iris.task)
#' pd = generatePartialDependenceData(fit, iris.task, "Petal.Width")
#' plotPartialDependence(pd, data = getTaskData(iris.task))
#'
#' # simulated example with weights computed via the joint distribution
#' # in practice empirical weights could be constructed by estimating the joint
#' # density from the training data (the data arg to fun) and computing the probability
#' # of the prediction grid under this estimated density (the newdata arg) or
#' # by using something like data depth or outlier classification to weight the
#' # unusualness of points in arg newdata.
#' sigma = matrix(c(1, .5, .5, 1), 2, 2)
#' C = chol(sigma)
#' X = replicate(2, rnorm(100)) %*% C
#' alpha = runif(2, -1, 1)
#' y = X %*% alpha
#' df = data.frame(y, X)
#' tsk = makeRegrTask(data = df, target = "y")
#' fit = train("regr.svm", tsk)
#'
#' w.fun = function(x, newdata) {
#'  # compute multivariate normal density given sigma
#'  sigma = matrix(c(1, .5, .5, 1), 2, 2)
#'  dec = chol(sigma)
#'  tmp = backsolve(dec, t(newdata), transpose = TRUE)
#'  rss = colSums(tmp^2)
#'  logretval = -sum(log(diag(dec))) - 0.5 * ncol(newdata) * log(2 * pi) - 0.5 * rss
#'  w = exp(logretval)
#'  # weight prediction grid given probability of grid points under the joint
#'  # density
#'  sum(w * x) / sum(w)
#' }
#'
#' generatePartialDependenceData(fit, tsk, "X1", fun = w.fun)
#' @export
generatePartialDependenceData = function(obj, input, features,
  interaction = FALSE, derivative = FALSE, individual = FALSE, center = NULL,
  fun = mean, bounds = c(qnorm(.025), qnorm(.975)),
  resample = "none", fmin, fmax, gridsize = 10L, range = NULL, ...) {

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
    fmin = sapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      min(data[[x]], na.rm = TRUE), NA), simplify = FALSE)
  if (missing(fmax))
    fmax = sapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      max(data[[x]], na.rm = TRUE), NA), simplify = FALSE)
  assertList(fmin, len = length(features))
  if (!all(names(fmin) %in% features))
    stop("fmin must be a named list with an NA or value corresponding to each feature.")
  assertList(fmax, len = length(features))
  if (!all(names(fmax) %in% features))
    stop("fmax must be a named list with an NA or value corresponding to each feature.")
  assertCount(gridsize, positive = TRUE)

  if (is.null(range))
    rng = generateFeatureGrid(features, data, resample, gridsize, fmin, fmax)
  else
    rng = range
  if (length(features) > 1L & interaction)
    rng = expand.grid(rng)

  if (td$type == "regr")
    target = td$target
  else if (td$type == "classif") {
    if (length(td$class.levels) > 2L)
      target = td$class.levels
    else
      target = td$positive
  }  else
    target = "Risk"

  args = list(obj = obj, data = data, fun = fun, td = td, individual = individual,
    bounds = bounds, ...)

  if (length(features) > 1L & !interaction) {
    out = lapply(features, function(x) {
      args$features = x
      if (derivative) {
        args$bounds = NULL
        out = parallelMap(doPartialDerivativeIteration, x = rng[[x]], more.args = args)
        rng = as.data.frame(rng[[x]])
        colnames(rng) = x
        centerpred = NULL
      } else {
        rng = as.data.frame(rng[[x]])
        colnames(rng) = x
        args$rng = rng
        out = parallelMap(doPartialDependenceIteration, i = seq_len(nrow(rng)), more.args = args)
        if (!is.null(center) & individual)
          centerpred = as.data.frame(doPartialDependenceIteration(obj, data, center[, x, drop = FALSE],
            x, fun, td, 1, bounds = bounds, ...))
        else
          centerpred = NULL
      }
      if (!individual)
        doAggregatePartialDependence(out, td, target, x, rng)
      else
        doIndividualPartialDependence(out, td, nrow(data), rng, target, x, centerpred)
    })
    out = setDF(rbindlist(out, fill = TRUE))
  } else {
    args$features = features
    if (derivative) {
      args$bounds = NULL
      out = parallelMap(doPartialDerivativeIteration, x = rng[[features]], more.args = args)
      centerpred = NULL
      rng = as.data.frame(rng)
      colnames(rng) = features
    } else {
      rng = as.data.frame(rng)
      colnames(rng) = features
      args$rng = rng
      out = parallelMap(doPartialDependenceIteration, i = seq_len(nrow(rng)), more.args = args)
      if (!is.null(center) & individual)
        centerpred = as.data.frame(doPartialDependenceIteration(obj, data, center, features, fun, td, 1, bounds))
      else
        centerpred = NULL
    }
    if (!individual)
      out = doAggregatePartialDependence(out, td, target, features, rng)
    else
      out = doIndividualPartialDependence(out, td, nrow(data), rng, target, features, centerpred)
  }

  if (td$type %in% c("regr", "surv"))
    out = out[, c(target, features, colnames(out)[!colnames(out) %in% c(target, features)])]
  else
    out = out[, c("Class", "Probability", features,
      colnames(out)[!colnames(out) %in% c("Class", "Probability", features)])]

  makeS3Obj("PartialDependenceData",
    data = out,
    task.desc = td,
    target = target,
    features = features,
    interaction = interaction,
    derivative = derivative,
    individual = individual,
    center = !is.null(center))
}
#' @title Generate a functional ANOVA decomposition
#'
#' @description
#' Decompose a learned prediction function as a sum of components estimated via partial dependence.
#'
#' @family partial_dependence functional_anova
#' @family generate_plot_data
#' @aliases FunctionalANOVAData
#'
#' @param obj [\code{\link{WrappedModel}}]\cr
#'   Result of \code{\link{train}}.
#' @param input [\code{data.frame} | \code{\link{Task}}]\cr
#'   Input data.
#' @param features [\code{character}]\cr
#'   A vector of feature names contained in the training data.
#'   If not specified all features in the \code{input} will be used.
#' @param depth [\code{integer(1)}]\cr
#'   An integer indicating the depth of interaction amongst the features to compute. Default 1.
#' @param fun [\code{function}]\cr
#'   A function that accepts a numeric vector and returns either a single number
#'   such as a measure of location such as the mean, or three numbers, which give a lower bound,
#'   a measure of location, and an upper bound. Note if three numbers are returned they must be
#'   in this order. Two variables, \code{data} and \code{newdata} are made available to \code{fun} internally via a
#'   wrapper. `data` is the training data from `input` and `newdata` contains a single point from the
#'   prediction grid for \code{features} along with the training data for features not in \code{features}.
#'   This allows the computation of weights based on comparisons of the prediction grid to the training data.
#'   The default is the mean.
#' @param bounds [\code{numeric(2)}]\cr
#'   The value (lower, upper) the estimated standard error is multiplied by to estimate the bound on a
#'   confidence region for a partial dependence. Ignored if \code{predict.type != "se"} for the learner.
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
#'   length of the evenly spaced grid created. Default 10.
#' @param ... additional arguments to be passed to \code{\link{predict}}.
#' @return [\code{FunctionalANOVAData}]. A named list, which contains the computed effects of the specified
#'   depth amongst the features.
#'
#' Object members:
#'   \item{data}{[\code{data.frame}]\cr
#'     Has columns for the prediction: one column for regression and an additional two if bounds are used.
#'     The \dQuote{effect} column specifies which features the prediction corresponds to.}
#'   \item{task.desc}{[\code{\link{TaskDesc}}]\cr
#'     Task description.}
#'   \item{target}{The target feature for regression.}
#'   \item{features}{[\code{character}]\cr
#'     Features argument input.}
#'   \item{interaction}{[\code{logical(1)}]\cr
#'     Whether or not the \code{depth} is greater than 1.}
#' @references
#' Giles Hooker, \dQuote{Discovering additive structure in black box functions.} Proceedings of the 10th ACM SIGKDD international conference on Knowledge discovery and data mining (2004): 575-580.
#' @examples
#' fit = train("regr.rpart", bh.task)
#' fa = generateFunctionalANOVAData(fit, bh.task, c("lstat", "crim"), depth = 2L)
#' plotPartialDependence(fa)
#' @export
generateFunctionalANOVAData = function(obj, input, features, depth = 1L, fun = mean,
  bounds = c(qnorm(.025), qnorm(.975)),
  resample = "none", fmin, fmax, gridsize = 10L, ...) {

  assertClass(obj, "WrappedModel")
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

  if (!td$type == "regr")
    stop("only regression tasks are permitted")
  excluded = colnames(data)
  excluded = excluded[!excluded %in% c(features, td$target)]
  assertChoice(resample, c("none", "bootstrap", "subsample"))

  if (missing(fmin))
    fmin = sapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      min(data[[x]], na.rm = TRUE), NA), simplify = FALSE)
  if (missing(fmax))
    fmax = sapply(features, function(x) ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      max(data[[x]], na.rm = TRUE), NA), simplify = FALSE)
  assertList(fmin, len = length(features))
  if (!all(names(fmin) %in% features))
    stop("fmin must be a named list with an NA or value corresponding to each feature.")
  assertList(fmax, len = length(features))
  if (!all(names(fmax) %in% features))
    stop("fmax must be a named list with an NA or value corresponding to each feature.")
  assertCount(gridsize, positive = TRUE)
  assertIntegerish(depth, lower = 1L, upper = length(features), len = 1L)

  assertFunction(fun)
  assertNumeric(bounds, len = 2L)
  assertNumber(bounds[1], upper = 0)
  assertNumber(bounds[2], lower = 0)

  ## generate grid
  fixed = generateFeatureGrid(features, data, resample, gridsize, fmin, fmax)

  ## generate list of effects to evaluate and associate with grid
  U = unlist(lapply(1:depth, function(x) combn(features, x, simplify = FALSE)), recursive = FALSE)
  depths = sapply(U, length)

  effects = sapply(U, function(u) stri_paste(u, collapse = ":"))
  fixed.grid = lapply(U, function(u) expand.grid(fixed[u]))
  names(fixed.grid) = effects

  target = td$target

  ## generate each effect
  pd = lapply(U, function(u, args) {
    args$features = u
    args$rng = fixed.grid[[stri_paste(u, collapse = ":")]]
    out = parallelMap(doPartialDependenceIteration, i = seq_len(nrow(args$rng)), more.args = args)
    doAggregatePartialDependence(out, td, target, u, args$rng)
  }, args = list(obj = obj, data = data, fun = fun, td = td, bounds = bounds, ...))
  names(pd) = effects

  if (all(c("upper", "lower") %in% colnames(pd[[1]])) |
        obj$learner$predict.type == "se")
    target = c("upper", "lower", target)

  ## remove lower order effects
  f = lapply(U, function(u) {
    hoe = pd[[stri_paste(u, collapse = ":")]]
    if (length(u) > 1) {
      sub = combn(u, length(u) - 1, simplify = FALSE)
      loe = lapply(pd[unlist(sub)], function(x) {
        to.match = colnames(x)[!(colnames(x) %in% target)]
        out = merge(x, hoe[, to.match, drop = FALSE], by = to.match)
        out[, colnames(out) %in% target]
      })
      hoe[, target] = hoe[, target] - Reduce("+", loe)
    }
    hoe
  })
  names(f) = effects

  makeS3Obj(c("FunctionalANOVAData", "PartialDependenceData"),
    data = setDF(rbindlist(f[depths == depth], fill = TRUE, idcol = "effect")),
    task.desc = td,
    target = target[!target %in% c("upper", "lower")],
    features = features,
    interaction = depth > 1L,
    derivative = FALSE,
    individual = FALSE,
    center = FALSE)
}

#' @export
print.FunctionalANOVAData = function(x, ...) {
  catf("FunctionalANOVAData")
  catf("Task: %s", x$task.desc$id)
  catf("Features: %s", stri_paste(x$features, collapse = ", "))
  catf("Target: %s", stri_paste(x$target, collapse = ", "))
  catf("Interaction Depth: %s", x$depth)
  catf("Effects Computed: %s", stri_paste(levels(x$data$effect), collapse = ", "))
  printHead(x$data, ...)
}

doPartialDerivativeIteration = function(x, obj, data, features, fun, td, individual, ...) {
  fun.wrapper = function(x, newdata, data, ...) {
    args = formals(fun)
    if (all(c("newdata", "data") %in% names(args))) {
      fun(x, newdata = newdata, data = data, ...)
    } else if ("newdata" %in% names(args)) {
      fun(x, newdata = newdata, ...)
    } else if ("data" %in% names(args)) {
        fun(x, data = data, ...)
    } else {
      fun(x, ...)
    }
  }

  f = function(x, obj, data, features, fun, td, ...) {
    newdata = data
    newdata[features] = x
    pred = do.call("predict", c(list("object" = obj, "newdata" = newdata), list(...)))
    if (obj$learner$predict.type == "response")
      fun(getPredictionResponse(pred), ...)
    else if (length(obj$task.desc$class.levels) == 2L)
      fun(getPredictionProbabilities(pred), ...)
    else
      apply(getPredictionProbabilities(pred), 2, fun, ...)
  }

  if (!individual) {
    # construct function appropriate for numDeriv w/ aggregate predictions
    if (obj$learner$predict.type == "response")
      numDeriv::grad(func = f, x = x, obj = obj, data = data, features = features, fun = fun.wrapper, td = td, ...)
    else
      t(numDeriv::jacobian(func = f, x = x, obj = obj, data = data, features = features, fun = fun.wrapper, td = td, ...))
  } else {
    if (obj$learner$predict.type == "response")
      sapply(seq_len(nrow(data)), function(idx)
        numDeriv::grad(func = f, x = x, obj = obj, data = data[idx, , drop = FALSE],
          features = features, fun = fun.wrapper, td = td, ...))
    else
      t(sapply(seq_len(nrow(data)), function(idx) numDeriv::jacobian(func = f, x = x, obj = obj,
        data = data[idx, , drop = FALSE], features = features, fun = fun.wrapper, td = td, ...)))
  }
}

doPartialDependenceIteration = function(obj, data, rng, features, fun, td, i, bounds, individual = FALSE, ...) {
  newdata = data
  newdata[features] = rng[i, ]
  fun.wrapper = function(x, newdata, data, ...) {
    args = formals(fun)
    if (all(c("newdata", "data") %in% names(args))) {
      fun(x, newdata = newdata, data = data, ...)
    } else if ("newdata" %in% names(args)) {
      fun(x, newdata = newdata, ...)
    } else if ("data" %in% names(args)) {
      fun(x, data = data, ...)
    } else {
      fun(x, ...)
    }
  }
  pred = do.call("predict", c(list("object" = obj, "newdata" = newdata), list(...)))
  if (obj$learner$predict.type == "response") {
    fun.wrapper(getPredictionResponse(pred), newdata, data, ...)
  } else if (length(obj$task.desc$class.levels) == 2L) {
    fun.wrapper(getPredictionProbabilities(pred), newdata, data, ...)
  } else if (obj$learner$predict.type == "se") {
    point = getPredictionResponse(pred)
    out = cbind(point + outer(getPredictionSE(pred), bounds), point)[, c(1, 3, 2)]
    unname(apply(out, 2, fun.wrapper, newdata = newdata, data = data, ...))
  } else {
    apply(getPredictionProbabilities(pred), 2, fun.wrapper, newdata = newdata,
      data = data, ...)
  }
}

doAggregatePartialDependence = function(out, td, target, features, rng) {
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
    out = melt(out, id.vars = features, variable = "Class", value.name = "Probability", variable.factor = TRUE)
    out$Class = stri_replace_all(out$Class, "", regex = "^prob\\.")
  }
  out
}

doIndividualPartialDependence = function(out, td, n, rng, target, features, centerpred = NULL) {
  if (td$type == "classif" & length(td$class.levels) > 2L) {
    if (!is.null(centerpred))
      out = lapply(out, function(x) x - centerpred) else
        out = lapply(out, as.data.frame)
    out = as.data.frame(rbindlist(out))
    colnames(out) = target
    idx = rep(seq_len(n), nrow(rng))
    rng = rng[rep(seq_len(nrow(rng)), each = n), , drop = FALSE]
    out = cbind(out, rng, idx, row.names = NULL)
    out = melt(out, id.vars = c(features, "idx"),
      variable.name = "Class", value.name = "Probability", variable.factor = TRUE)
    out$idx = interaction(out$idx, out$Class)
  } else {
    out = as.data.frame(setDT(transpose(out))) # see https://github.com/Rdatatable/data.table/issues/600
    if (!is.null(centerpred))
      out = out - setDF(transpose(rep(centerpred, nrow(out)))) #t(as.data.frame(lapply(out, function(x) unname(x - centerpred))))
    colnames(out) = 1:n
    out = cbind(out, rng)
    out = melt(out, id.vars = features, variable.name = "idx", value.name = target)
    if (td$type == "classif")
      out = melt(out, id.vars = c(features, "idx"), value.name = "Probability",
        variable.name = "Class", variable.factor = TRUE)
  }
  out
}

#' @export
print.PartialDependenceData = function(x, ...) {
  catf("PartialDependenceData")
  catf("Task: %s", x$task.desc$id)
  catf("Features: %s", stri_paste(x$features, collapse = ", ", sep = " "))
  catf("Target: %s", stri_paste(x$target, collapse = ", ", sep = " "))
  catf("Derivative: %s", x$derivative)
  catf("Interaction: %s", x$interaction)
  catf("Individual: %s", x$individual)
  if (x$individual)
    catf("Predictions centered: %s", x$center)
  printHead(x$data, ...)
}
#' @title Plot a partial dependence with ggplot2.
#' @description
#' Plot a partial dependence from \code{\link{generatePartialDependenceData}} using ggplot2.
#'
#' @family partial_dependence
#' @family plot
#'
#' @param obj [\code{PartialDependenceData}]\cr
#'   Generated by \code{\link{generatePartialDependenceData}}.
#' @param geom [\code{charater(1)}]\cr
#'   The type of geom to use to display the data. Can be \dQuote{line} or \dQuote{tile}.
#'   For tiling at least two features must be used with \code{interaction = TRUE} in the call to
#'   \code{\link{generatePartialDependenceData}}. This may be used in conjuction with the
#'   \code{facet} argument if three features are specified in the call to
#'   \code{\link{generatePartialDependenceData}}.
#'   Default is \dQuote{line}.
#' @param facet [\code{character(1)}]\cr
#'   The name of a feature to be used for facetting.
#'   This feature must have been an element of the \code{features} argument to
#'   \code{\link{generatePartialDependenceData}} and is only applicable when said argument had length
#'   greater than 1.
#'   The feature must be a factor or an integer.
#'   If \code{\link{generatePartialDependenceData}} is called with the \code{interaction} argument \code{FALSE}
#'   (the default) with argument \code{features} of length greater than one, then \code{facet} is ignored and
#'   each feature is plotted in its own facet.
#'   Default is \code{NULL}.
#' @template arg_facet_nrow_ncol
#' @param p [\code{numeric(1)}]\cr
#'   If \code{individual = TRUE} then \code{sample} allows the user to sample without replacement
#'   from the output to make the display more readable. Each row is sampled with probability \code{p}.
#'   Default is \code{1}.
#' @param data [\code{data.frame}]\cr
#'   Data points to plot. Usually the training data. For survival and binary classification tasks a rug plot
#'   wherein ticks represent failures or instances of the positive class are shown. For regression tasks
#'   points are shown. For multiclass classification tasks ticks are shown and colored according to their class.
#'   Both the features and the target must be included.
#'   Default is \code{NULL}.
#' @template ret_gg2
#' @export
plotPartialDependence = function(obj, geom = "line", facet = NULL, facet.wrap.nrow = NULL,
  facet.wrap.ncol = NULL, p = 1, data = NULL) {

  assertClass(obj, "PartialDependenceData")
  assertChoice(geom, c("tile", "line"))
  if (obj$interaction & length(obj$features) > 2L & geom != "tile")
    stop("Cannot plot more than 2 features together with line plots.")
  if (geom == "tile") {
    if (!obj$interaction)
      stop("obj argument created by generatePartialDependenceData was called with interaction = FALSE!")
  }

  if ("FunctionalANOVAData" %in% class(obj)) {
    if (length(unique(obj$data$effect)) > 1L & obj$interaction)
      stop("Cannot plot multiple ANOVA effects of depth > 1.")
  }

  if (!is.null(data)) {
    assertDataFrame(data, col.names = "unique", min.rows = 1L,
                    min.cols = length(obj$features) + length(obj$td$target))
    assertSubset(obj$features, colnames(data), empty.ok = FALSE)
  }

  if (!is.null(facet)) {
    assertChoice(facet, obj$features)
    if (!length(obj$features) %in% 2:3)
      stop("obj argument created by generatePartialDependenceData must be called with two or three features to use this argument!")
    if (!obj$interaction)
      stop("obj argument created by generatePartialDependenceData must be called with interaction = TRUE to use this argument!")

    features = obj$features[which(obj$features != facet)]

    if (is.factor(obj$data[[facet]])) {
      obj$data[[facet]] = stri_paste(facet, "=", obj$data[[facet]], sep = " ")
    } else if (is.character(obj$data[[facet]])) {
      obj$data[[facet]] = stri_paste(facet, "=", as.factor(obj$data[[facet]]), sep = " ")
    } else if (is.numeric(obj$data[[facet]])) {
      obj$data[[facet]] = stri_paste(facet, "=", as.factor(signif(obj$data[[facet]], 3L)), sep = " ")
    } else {
      stop("Invalid input to facet arg. Must refer to a numeric/integer, character, or facet feature.")
    }

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
      stop("obj argument created by generatePartialDependenceData must be called with individual = TRUE to use this argument!")
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
    idx = which(sapply(obj$data, class) == "factor" & colnames(obj$data) %in% features)
    # explicit casting previously done implicitly by reshape2::melt.data.frame
    for (id in idx) obj$data[, id] = as.numeric(obj$data[, id])
    obj$data = setDF(melt(data.table(obj$data),
      id.vars = colnames(obj$data)[!colnames(obj$data) %in% features],
      variable = "Feature", value.name = "Value", na.rm = TRUE, variable.factor = TRUE))
    if (!obj$individual) {
      if (obj$task.desc$type %in% c("regr", "surv"))
        plt = ggplot(obj$data, aes_string("Value", target)) +
          geom_line(color = ifelse(is.null(data), "black", "red")) + geom_point()
      else
        plt = ggplot(obj$data, aes_string("Value", "Probability", group = "Class", color = "Class")) +
          geom_line() + geom_point()
    } else {
      if (obj$task.desc$type %in% c("regr", "surv")) {
        plt = ggplot(obj$data, aes_string("Value", target, group = "idx")) +
          geom_line(alpha = .25, color = ifelse(is.null(data), "black", "red")) + geom_point()
      } else {
        plt = ggplot(obj$data, aes_string("Value", "Probability", group = "idx", color = "Class")) +
          geom_line(alpha = .25) + geom_point()
      }
    }

    if (length(features) == 1L) {
      if (obj$task.desc$type %in% c("regr", "surv"))
        plt = plt + labs(x = features, y = target)
      else
        plt = plt + labs(x = features)
    }

    # bounds from fun or se estimation
    if (bounds)
      plt = plt + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = .5)

    # labels for ice plots
    if (obj$center)
      plt = plt + ylab(stri_paste(target, "(centered)", sep = " "))

    if (obj$derivative)
      plt = plt + ylab(stri_paste(target, "(derivative)", sep = " "))

  } else { ## tiling
    if (obj$task.desc$type == "classif") {
      target = "Probability"
      facet = "Class"
      scales = "free"
    }
    plt = ggplot(obj$data, aes_string(x = features[1], y = features[2], fill = target))
    plt = plt + geom_raster(aes_string(fill = target))

    # labels for ICE plots
    if (obj$center)
      plt = plt + scale_fill_continuous(guide = guide_colorbar(title = stri_paste(target, "(centered)", sep = " ")))

    if (obj$derivative)
      plt = plt + scale_fill_continuous(guide = guide_colorbar(title = stri_paste(target, "(derivative)", sep = " ")))
  }

  # facetting
  if (!is.null(facet)) {
    plt = plt + facet_wrap(as.formula(stri_paste("~", facet)), scales = scales,
      nrow = facet.wrap.nrow, ncol = facet.wrap.ncol)
  }

  # data overplotting
  if (!is.null(data)) {
    data = data[, colnames(data) %in% c(obj$features, obj$task.desc$target)]
    if (!is.null(facet)) {
      if (!facet %in% obj$features)
        data = melt(data, id.vars = c(obj$task.desc$target, obj$features[obj$features == facet]),
                    variable = "Feature", value.name = "Value", na.rm = TRUE, variable.factor = TRUE)
      if (facet %in% obj$features) {
        if (!is.factor(data[[facet]]))
          data[[facet]] = stri_paste(facet, "=", as.factor(signif(data[[facet]], 2)), sep = " ")
        else
          data[[facet]] = stri_paste(facet, "=", data[[facet]], sep = " ")
      }
    }

    if (geom == "line") {
      if (obj$task.desc$type %in% c("classif", "surv")) {
        if (obj$task.desc$type == "classif") {
          if (!is.na(obj$task.desc$positive)) {
            plt = plt + geom_rug(aes_string(plt$labels$x, color = obj$task.desc$target),
                                 data[data[[obj$task.desc$target]] == obj$task.desc$positive, ],
                                 alpha = .25, inherit.aes = FALSE)
          } else {
            plt = plt + geom_rug(aes_string(plt$labels$x), data, alpha = .25, inherit.aes = FALSE)
          }
        } else {
          plt = plt + geom_rug(aes_string(plt$labels$x),
                               data[data[[obj$task.desc$target[2]]], ],
                               alpha = .25, inherit.aes = FALSE)
        }
      } else {
        plt = plt + geom_point(aes_string(plt$labels$x, obj$task.desc$target),
                               data, alpha = .25, inherit.aes = FALSE)
      }
    } else {
      plt = plt + geom_point(aes_string(plt$labels$x, plt$labels$y), data, alpha = .25, inherit.aes = FALSE)
    }
  }

  plt
}
#' @title Plot a partial dependence using ggvis.
#' @description
#' Plot partial dependence from \code{\link{generatePartialDependenceData}} using ggvis.
#'
#' @family partial_dependence
#' @family plot
#'
#' @param obj [\code{PartialDependenceData}]\cr
#'   Generated by \code{\link{generatePartialDependenceData}}.
#' @param interact [\code{character(1)}]\cr
#'   The name of a feature to be mapped to an interactive sidebar using Shiny.
#'   This feature must have been an element of the \code{features} argument to
#'   \code{\link{generatePartialDependenceData}} and is only applicable when said argument had length
#'   greater than 1.
#'   If \code{\link{generatePartialDependenceData}} is called with the \code{interaction} argument \code{FALSE}
#'   (the default) with argument \code{features} of length greater than one, then \code{interact} is ignored and
#'   the feature displayed is controlled by an interactive side panel.
#'   Default is \code{NULL}.
#' @param p [\code{numeric(1)}]\cr
#'   If \code{individual = TRUE} then \code{sample} allows the user to sample without replacement
#'   from the output to make the display more readable. Each row is sampled with probability \code{p}.
#'   Default is \code{1}.
#' @template ret_ggv
#' @export
plotPartialDependenceGGVIS = function(obj, interact = NULL, p = 1) {
  requirePackages("_ggvis")
  assertClass(obj, "PartialDependenceData")
  if (!is.null(interact))
    assertChoice(interact, obj$features)
  if (obj$interaction & length(obj$features) > 2L)
    stop("It is only possible to plot 2 features with this function.")

  if (!obj$interaction & !is.null(interact))
    stop("obj argument created by generatePartialDependenceData was called with interaction = FALSE!")

  if (p != 1) {
    assertNumber(p, lower = 0, upper = 1, finite = TRUE)
    if (!obj$individual)
      stop("obj argument created by generatePartialDependenceData must be called with individual = TRUE to use this argument!")
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
    obj$data = melt(obj$data, id.vars = id, variable.name = "Feature",
                    value.name = "Value", na.rm = TRUE, variable.factor = TRUE)
    interact = "Feature"
    choices = obj$features
  } else
    interact = NULL

  bounds = all(c("lower", "upper") %in% colnames(obj$data) & obj$task.desc$type %in% c("surv", "regr"))

  if (obj$task.desc$type %in% c("regr", "classif"))
    target = obj$task.desc$target
  else
    target = "Risk"

  createPlot = function(td, target, interaction, individual, data, x, bounds) {
    classif = td$type == "classif" & all(target %in% td$class.levels)
    if (classif) {
      if (interaction)
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name(x)),
          ggvis::prop("y", as.name("Probability")),
          ggvis::prop("stroke", as.name("Class")))
      else if (!interaction & !is.null(interact)) ## no interaction but multiple features
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name("Value")),
          ggvis::prop("y", as.name("Probability")),
          ggvis::prop("stroke", as.name("Class")))
      else
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name(x)),
          ggvis::prop("y", as.name("Probability")),
          ggvis::prop("stroke", as.name("Class")))

    } else { ## regression/survival
      if (interaction)
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name(x)),
          ggvis::prop("y", as.name(target)))
      else if (!interaction & !is.null(interact))
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name("Value")),
          ggvis::prop("y", as.name(target)))
      else
        plt = ggvis::ggvis(data,
          ggvis::prop("x", as.name(x)),
          ggvis::prop("y", as.name(target)))
    }

    if (bounds)
      plt = ggvis::layer_ribbons(plt,
        ggvis::prop("y", as.name("lower")),
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
    header = stri_paste(target, "(centered)", sep = " ")
  else if (obj$derivative)
    header = stri_paste(target, "(derivative)", sep = " ")
  else
    header = target

  if (!is.null(interact)) {
    requirePackages("_shiny")
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
      plt = shiny::reactive(createPlot(obj$task.desc, obj$target, obj$interaction, obj$individual,
          obj$data[obj$data[[interact]] == input$interaction_select, ],
          x, bounds))
      ggvis::bind_shiny(plt, "ggvis", "ggvis_ui")
      })
    shiny::shinyApp(ui, server)
  } else
    createPlot(obj$task.desc, obj$target, obj$interaction, obj$individual, obj$data, obj$features, bounds)
}
