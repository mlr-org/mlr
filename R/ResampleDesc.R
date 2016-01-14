#' @title Create a description object for a resampling strategy.
#'
#' @description
#' A description of a resampling algorithm contains all necessary information to
#' create a \code{\link{ResampleInstance}}, when given the size of the data set.
#'
#' @details
#' Some notes on some special strategies:
#' \describe{
#' \item{Repeated cross-validation}{Use \dQuote{RepCV}. Then you have to set the aggregation function
#'   for your preferred performance measure to \dQuote{testgroup.mean}
#'   via \code{\link{setAggregation}}.}
#' \item{B632 bootstrap}{Use \dQuote{Bootstrap} for bootstrap and set predict to \dQuote{both}.
#'   Then you have to set the aggregation function for your preferred performance measure to
#'   \dQuote{b632} via \code{\link{setAggregation}}.}
#' \item{B632+ bootstrap}{Use \dQuote{Bootstrap} for bootstrap and set predict to \dQuote{both}.
#'   Then you have to set the aggregation function for your preferred performance measure to
#'   \dQuote{b632plus} via \code{\link{setAggregation}}.}
#' \item{Fixed Holdout set}{Use \code{\link{makeFixedHoldoutInstance}}.}
#' }
#'
#' Object slots:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of resampling strategy.}
#' \item{iters [\code{integer(1)}]}{Number of iterations. Note that this is always the complete number
#'   of generated train/test sets, so for a 10-times repeated 5fold cross-validation it would be 50.}
#' \item{predict [\code{character(1)}]}{See argument.}
#' \item{stratify [\code{logical(1)}]}{See argument.}
#' \item{All parameters passed in ... under the respective argument name}{See arguments.}
#' }
#'
#' @param method [\code{character(1)}]\cr
#'   \dQuote{CV} for cross-validation, \dQuote{LOO} for leave-one-out, \dQuote{RepCV} for
#'   repeated cross-validation, \dQuote{Bootstrap} for out-of-bag bootstrap, \dQuote{Subsample} for
#'   subsampling, \dQuote{Holdout} for holdout.
#' @param predict [\code{character(1)}]\cr
#'   What to predict during resampling: \dQuote{train}, \dQuote{test} or \dQuote{both} sets.
#'   Default is \dQuote{test}.
#' @param ... [any]\cr
#'   Further parameters for strategies.\cr
#'   \describe{
#'   \item{iters [\code{integer(1)}]}{Number of iterations, for \dQuote{CV}, \dQuote{Subsample}
#'     and \dQuote{Boostrap}.}
#'   \item{split [\code{numeric(1)}]}{Proportion of training cases for \dQuote{Holdout} and
#'     \dQuote{Subsample} between 0 and 1. Default is 2/3.}
#'   \item{reps [\code{integer(1)}]}{Repeats for \dQuote{RepCV}. Here \code{iters = folds * reps}.
#'     Default is 10.}
#'   \item{folds [\code{integer(1)]}}{Folds in the repeated CV for \code{RepCV}.
#'     Here \code{iters = folds * reps}. Default is 10.}
#'   }
#' @param stratify [\code{logical(1)}]\cr
#'   Should stratification be done for the target variable?
#'   For classification tasks, this means that the resampling strategy is applied to all classes
#'   individually and the resulting index sets are joined to make sure that the proportion of
#'   observations in each training set is as in the original data set. Useful for imbalanced class sizes.
#'   For survival tasks stratification is done on the events, resulting in training sets with comparable
#'   censoring rates.
#' @param stratify.cols [\code{character}]\cr
#'   Stratify on specific columns referenced by name. All columns have to be factors.
#'   Note that you have to ensure yourself that stratification is possible, i.e.
#'   that each strata contains enough observations.
#'   This argument and \code{stratify} are mutually exclusive.
#' @return [\code{\link{ResampleDesc}}].
#' @family resample
#' @export
#' @aliases ResampleDesc
#' @examples
#' # Bootstraping
#' makeResampleDesc("Bootstrap", iters = 10)
#' makeResampleDesc("Bootstrap", iters = 10, predict = "both")
#'
#' # Subsampling
#' makeResampleDesc("Subsample", iters = 10, split = 3/4)
#' makeResampleDesc("Subsample", iters = 10)
#'
#' # Holdout a.k.a. test sample estimation
#' makeResampleDesc("Holdout")
makeResampleDesc = function(method, predict = "test", ..., stratify = FALSE, stratify.cols = NULL) {
  assertChoice(method, choices = c("DPS", "Holdout", "CV", "LOO",  "RepCV", "Subsample", "Bootstrap"))
  assertChoice(predict, choices = c("train", "test", "both"))
  assertFlag(stratify)
  if (stratify && method == "LOO")
    stop("Stratification cannot be done for LOO!")
  if (stratify && ! is.null(stratify.cols))
    stop("Arguments 'stratify' and 'stratify.cols' are mutually exclusive!")
  d = do.call(paste0("makeResampleDesc", method), list(...))
  d$predict = predict
  d$stratify = stratify
  d$stratify.cols = stratify.cols
  addClasses(d, paste0(method, "Desc"))
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
makeResampleDescDPS = function(iters = 8L) {
  iters = asCount(iters, positive = TRUE)
  k = log2(iters)
  if (as.integer(k) != k)
    stopf("'iters' must be a power of 2, but it is %i!", iters)
  makeResampleDescInternal("density preserving sampling", iters = iters)
}

makeResampleDescHoldout = function(iters, split = 2/3) {
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("holdout", iters = 1L, split = split)
}

makeResampleDescCV = function(iters = 10L) {
  iters = asInt(iters, lower = 2L)
  makeResampleDescInternal("cross-validation", iters = iters)
}

makeResampleDescLOO = function() {
  makeResampleDescInternal("LOO", iters = NA_integer_)
}

makeResampleDescSubsample = function(iters = 30L, split = 2/3) {
  iters = asCount(iters, positive = TRUE)
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("subsampling", iters = iters, split = split)
}

makeResampleDescBootstrap = function(iters = 30L) {
  iters = asCount(iters, positive = TRUE)
  makeResampleDescInternal("OOB bootstrapping", iters = iters)
}

makeResampleDescRepCV = function(reps = 10L, folds = 10L) {
  reps = asInt(reps, lower = 2L)
  folds = asInt(folds, lower = 2L)
  makeResampleDescInternal("repeated cross-validation", iters = folds*reps, folds = folds, reps = reps)
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
    x$id, x$iters, x$iters/x$reps, x$reps)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

