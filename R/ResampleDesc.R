#' @title Create a description object for a resampling strategy.
#'
#' @description
#' A description of a resampling algorithm contains all necessary information to
#' create a [ResampleInstance], when given the size of the data set.
#'
#' @details
#' Some notes on some special strategies:
#' \describe{
#' \item{Repeated cross-validation}{Use \dQuote{RepCV}. Then you have to set the aggregation function
#'   for your preferred performance measure to \dQuote{testgroup.mean}
#'   via [setAggregation].}
#' \item{B632 bootstrap}{Use \dQuote{Bootstrap} for bootstrap and set predict to \dQuote{both}.
#'   Then you have to set the aggregation function for your preferred performance measure to
#'   \dQuote{b632} via [setAggregation].}
#' \item{B632+ bootstrap}{Use \dQuote{Bootstrap} for bootstrap and set predict to \dQuote{both}.
#'   Then you have to set the aggregation function for your preferred performance measure to
#'   \dQuote{b632plus} via [setAggregation].}
#' \item{Fixed Holdout set}{Use [makeFixedHoldoutInstance].}
#' }
#'
#' Object slots:
#' \describe{
#' \item{id (`character(1)`)}{Name of resampling strategy.}
#' \item{iters (`integer(1)`)}{Number of iterations. Note that this is always the complete number
#'   of generated train/test sets, so for a 10-times repeated 5fold cross-validation it would be 50.}
#' \item{predict (`character(1)`)}{See argument.}
#' \item{stratify (`logical(1)`)}{See argument.}
#' \item{All parameters passed in ... under the respective argument name}{See arguments.}
#' }
#'
#' @param method (`character(1)`)\cr
#'   \dQuote{CV} for cross-validation, \dQuote{LOO} for leave-one-out, \dQuote{RepCV} for
#'   repeated cross-validation, \dQuote{Bootstrap} for out-of-bag bootstrap, \dQuote{Subsample} for
#'   subsampling, \dQuote{Holdout} for holdout, \dQuote{GrowingWindowCV} for growing window
#'   cross-validation, \dQuote{FixedWindowCV} for fixed window cross validation.
#' @param predict (`character(1)`)\cr
#'   What to predict during resampling: \dQuote{train}, \dQuote{test} or \dQuote{both} sets.
#'   Default is \dQuote{test}.
#' @param ... (any)\cr
#'   Further parameters for strategies.\cr
#'   \describe{
#'   \item{iters (`integer(1)`)}{Number of iterations, for \dQuote{CV}, \dQuote{Subsample}
#'     and \dQuote{Bootstrap}.}
#'   \item{split (`numeric(1)`)}{Proportion of training cases for \dQuote{Holdout} and
#'     \dQuote{Subsample} between 0 and 1. Default is 2 / 3.}
#'   \item{reps (`integer(1)`)}{Repeats for \dQuote{RepCV}. Here `iters = folds * reps`.
#'     Default is 10.}
#'   \item{folds (`integer(1)`)}{Folds in the repeated CV for `RepCV`.
#'     Here `iters = folds * reps`. Default is 10.}
#'   \item{horizon (`numeric(1)`)}{Number of observations in the forecast test set for \dQuote{GrowingWindowCV}
#'    and \dQuote{FixedWindowCV}. When `horizon > 1` this will be treated as the number of
#'    observations to forecast, else it will be a fraction of the initial window. IE,
#'    for 100 observations, initial window of .5, and horizon of .2, the test set will have
#'    10 observations. Default is 1.}
#'   \item{initial.window (`numeric(1)`)}{Fraction of observations to start with
#'    in the training set for \dQuote{GrowingWindowCV} and \dQuote{FixedWindowCV}.
#'    When `initial.window > 1` this will be treated as the number of
#'    observations in the initial window, else it will be treated as the fraction
#'    of observations to have in the initial window. Default is 0.5.}
#'   \item{skip (`numeric(1)`)}{ How many resamples to skip to thin the total amount
#'    for \dQuote{GrowingWindowCV} and \dQuote{FixedWindowCV}. This is passed through as the \dQuote{by} argument
#'    in `seq()`. When `skip > 1` this will be treated as the increment of the sequence of resampling indices,
#'     else it will be a fraction of the total training indices. IE for 100 training sets and a value of .2, the increment
#'     of the resampling indices will be 20. Default is \dQuote{horizon} which gives mutually exclusive chunks
#'      of test indices.}
#'   }
#' @param fixed (`logical(1)`)\cr
#'   Whether indices supplied via argument 'blocking' in the task should be used as
#'   fully pre-defined indices. Default is `FALSE` which means
#'   they will be used following the 'blocking' approach.
#'   `fixed` only works with ResampleDesc `CV` and the supplied indices must match
#'   the number of observations. When `fixed = TRUE`, the `iters` argument will be ignored
#'   and is interally set to the number of supplied factor levels in `blocking`.
#' @param blocking.cv (`logical(1)`)\cr
#'   Should 'blocking' be used in `CV`? Default to `FALSE`.
#'   This is different to `fixed = TRUE` and cannot be combined. Please check the mlr online tutorial
#'   for more details.
#' @param stratify (`logical(1)`)\cr
#'   Should stratification be done for the target variable?
#'   For classification tasks, this means that the resampling strategy is applied to all classes
#'   individually and the resulting index sets are joined to make sure that the proportion of
#'   observations in each training set is as in the original data set. Useful for imbalanced class sizes.
#'   For survival tasks stratification is done on the events, resulting in training sets with comparable
#'   censoring rates.
#' @param stratify.cols ([character])\cr
#'   Stratify on specific columns referenced by name. All columns have to be factor or integer.
#'   Note that you have to ensure yourself that stratification is possible, i.e.
#'   that each strata contains enough observations.
#'   This argument and `stratify` are mutually exclusive.
#' @return ([ResampleDesc]).
#' @family resample
#' @export
#' @aliases ResampleDesc
#' @examples
#' # Bootstraping
#' makeResampleDesc("Bootstrap", iters = 10)
#' makeResampleDesc("Bootstrap", iters = 10, predict = "both")
#'
#' # Subsampling
#' makeResampleDesc("Subsample", iters = 10, split = 3 / 4)
#' makeResampleDesc("Subsample", iters = 10)
#'
#' # Holdout a.k.a. test sample estimation
#' makeResampleDesc("Holdout")
makeResampleDesc = function(method, predict = "test", ..., stratify = FALSE,
  stratify.cols = NULL, fixed = FALSE, blocking.cv = FALSE) {

  assertChoice(method, choices = c("Holdout", "CV", "LOO", "RepCV",
    "Subsample", "Bootstrap", "SpCV", "SpRepCV",
    "GrowingWindowCV", "FixedWindowCV"))
  assertChoice(predict, choices = c("train", "test", "both"))
  assertFlag(stratify)
  if (stratify && method == "LOO") {
    stop("Stratification cannot be done for LOO!")
  }
  if (stratify && !is.null(stratify.cols)) {
    stop("Arguments 'stratify' and 'stratify.cols' are mutually exclusive!")
  }
  d = do.call(stri_paste("makeResampleDesc", method), list(...))
  d$predict = predict
  d$stratify = stratify
  d$stratify.cols = stratify.cols
  d$fixed = fixed
  d$blocking.cv = blocking.cv
  addClasses(d, stri_paste(method, "Desc"))
}


makeResampleDescInternal = function(id, iters, predict = "test", ...) {
  setClasses(insert(list(...), list(id = id, iters = iters, predict = predict)),
    "ResampleDesc")
}

#' @export
print.ResampleDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations.", x$id, x$iters)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

##############################################################################################
# all following constructors are only called INTERNALLY in makeResampleDesc
# note that stuff like the stratify flag are set in that super-constructor.
# the methods cannot be directly exported like this!
# FIXME: the code style is not so good here, see issue 187.
##############################################################################################

makeResampleDescHoldout = function(iters, split = 2 / 3) {
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("holdout", iters = 1L, split = split)
}

makeResampleDescCV = function(iters = 10L, fixed = FALSE, blocking.cv = FALSE) {
  iters = asInt(iters, lower = 2L)
  makeResampleDescInternal("cross-validation", iters = iters, fixed = fixed,
    blocking.cv = blocking.cv)
}

makeResampleDescSpCV = function(iters = 10L) {
  iters = asInt(iters, lower = 2L)
  makeResampleDescInternal("spatial cross-validation", iters = iters)
}

makeResampleDescLOO = function() {
  makeResampleDescInternal("LOO", iters = NA_integer_)
}

makeResampleDescSubsample = function(iters = 30L, split = 2 / 3) {
  iters = asCount(iters, positive = TRUE)
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("subsampling", iters = iters, split = split)
}

makeResampleDescBootstrap = function(iters = 30L) {
  iters = asCount(iters, positive = TRUE)
  makeResampleDescInternal("OOB bootstrapping", iters = iters)
}

makeResampleDescRepCV = function(reps = 10L, folds = 10L, fixed = FALSE, blocking.cv = FALSE) {
  reps = asInt(reps, lower = 2L)
  folds = asInt(folds, lower = 2L)
  makeResampleDescInternal("repeated cross-validation", iters = folds * reps, folds = folds, reps = reps,
    fixed = fixed, blocking.cv = blocking.cv)
}

makeResampleDescSpRepCV = function(reps = 10L, folds = 10L) {
  reps = asInt(reps, lower = 2L)
  folds = asInt(folds, lower = 2L)
  makeResampleDescInternal("repeated spatial cross-validation", iters = folds * reps, folds = folds, reps = reps)
}


makeResampleDescFixedWindowCV = function(horizon = 1L, initial.window = .5, skip = horizon - 1) {
  assertNumeric(horizon, lower = 0)
  assertNumeric(initial.window, lower = 0)
  assertNumeric(skip, lower = 0)
  makeResampleDescInternal("Fixed", iters = NA_integer_, horizon = horizon,
    initial.window = initial.window, skip = skip, stratify = FALSE)
}

makeResampleDescGrowingWindowCV = function(horizon = 1L, initial.window = .5, skip = horizon - 1) {
  assertNumeric(horizon, lower = 0)
  assertNumeric(initial.window, lower = 0)
  assertNumeric(skip, lower = 0)
  makeResampleDescInternal("Growing", iters = NA_integer_, horizon = horizon,
    initial.window = initial.window, skip = skip, stratify = FALSE)
}

##############################################################################################

#' @export
print.HoldoutDesc = function(x, ...) {
  catf("Resample description: %s with %.2f split rate.",
    x$id, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.SubsampleDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations and %.2f split rate.",
    x$id, x$iters, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.RepCVDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations: %i folds and %i reps.",
    x$id, x$iters, x$iters / x$reps, x$reps)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.GrowingWindowCVDesc = function(x, ...) {
  catf("Window description:\n %s: %.2f in initial window, horizon of %.2f, and skipping %.2f windows.",
    x$id, x$initial.window, x$horizon, x$skip)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.FixedWindowCVDesc = function(x, ...) {
  catf("Window description:\n %s: %.2f in initial window, horizon of %.2f, and skipping %.2f windows.",
    x$id, x$initial.window, x$horizon, x$skip)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

##############################################################################################
# Resample Convenience Objects, like cv10
##############################################################################################

#' @rdname makeResampleDesc
#' @section Standard ResampleDesc objects:
#' For common resampling strategies you can save some typing
#' by using the following description objects:
#' \describe{
#' \item{hout}{holdout a.k.a. test sample estimation
#' (two-thirds training set, one-third testing set)}
#' \item{cv2}{2-fold cross-validation}
#' \item{cv3}{3-fold cross-validation}
#' \item{cv5}{5-fold cross-validation}
#' \item{cv10}{10-fold cross-validation}
#' }
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
hout = makeResampleDesc("Holdout")

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv2 = makeResampleDesc("CV", iters = 2L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv3 = makeResampleDesc("CV", iters = 3L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv5 = makeResampleDesc("CV", iters = 5L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv10 = makeResampleDesc("CV", iters = 10L)
