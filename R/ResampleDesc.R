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
#'     and \dQuote{Boostrap}}
#'   \item{split [\code{numeric(1)}]}{Proportion of training cases for \dQuote{Holdout} and
#'     \dQuote{Subsample} between 0 and 1. Default is 2/3.}
#'   \item{reps [integer(1)]}{Repeats for \dQuote{RepCV}. Here \code{iters = folds * reps}.
#'     Default is 10.}
#'   \item{folds [integer(1)]}{Folds in the repeated CV for \code{RepCV}.
#'     Here \code{iters = folds * reps}. Default is 10.}
#'   }
#' @param stratify [\code{logical(1)}]\cr
#'   Should stratification be done for the classes in classification tasks?
#'   This means that the resampling strategy is applied to all classes individually and the resulting
#'   index sets are joined to make sure that the proportion of observations in each training set
#'   is as in the original data set. Useful for imbalanced class sizes.
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
makeResampleDesc = function(method, predict = "test", ..., stratify = FALSE) {
  assertChoice(method, choices = c("Holdout", "CV", "LOO",  "RepCV", "Subsample", "Bootstrap"))
  assertChoice(predict, choices = c("train", "test", "both"))
  assertFlag(stratify)
  if (stratify && method == "LOO")
    stop("Stratification cannot be done for LOO!")
  d = do.call(paste0("makeResampleDesc", method), list(...))
  d$predict = predict
  d$stratify = stratify
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
