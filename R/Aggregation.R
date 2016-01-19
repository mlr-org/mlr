#' @title Aggregation object.
#' @description
#' An aggregation method reduces the performance values of the test
#' (and possibly the training sets) to a single value.
#' To see all possible implemented aggregations look at \code{\link{aggregations}}.
#'
#' The aggregation can access all relevant information of the result after resampling
#' and combine them into a single value. Though usually something very simple
#' like taking the mean of the test set performances is done.
#'
#' Object members:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of the aggregation method.}
#' \item{name [\code{character(1)}]}{Long name of the aggregation method.}
#' \item{fun [\code{function(task, perf.test, perf.train, measure, group, pred)}]}{Aggregation function.}
#' }
#' @name Aggregation
#' @seealso \code{\link{makeAggregation}}
#' @rdname Aggregation
NULL


#' @title Specify your own aggregation of measures.
#'
#' @description
#' This is an advanced feature of mlr. It gives access to some
#' inner workings so the result might not be compatible with everything!
#'
#' @param id [\code{character(1)}]\cr
#'   Name of the aggregation method (preferably the same name as the generated function).
#' @param name [\code{character(1)}]\cr
#'   Long name of the aggregation method. Default is \code{id}.
#' @param fun [\code{function(task, perf.test, perf.train, measure, group, pred)}]\cr
#'   Calculates the aggregated performance. In most cases you will only need the performances
#'   \code{perf.test} and optionally \code{perf.train} on the test and training data sets.
#'   \describe{
#'     \item{\code{task} [\code{\link{Task}}]}{The task.}
#'     \item{\code{perf.test} [\code{numeric}]}{
#'       \code{\link{performance}} results on the test data sets.}
#'     \item{\code{perf.train} [\code{numeric}]}{
#'       \code{\link{performance}} results on the training data sets.}
#'     \item{\code{measure} [\code{\link{Measure}}]}{
#'       Performance measure.}
#'     \item{\code{group} [\code{factor}]}{
#'       Grouping of resampling iterations. This encodes whether specific iterations
#'       'belong together' (e.g. repeated CV).}
#'     \item{\code{pred} [\code{\link{Prediction}}]}{
#'       Prediction object.}
#'   }
#' @seealso \code{\link{aggregations}}, \code{\link{setAggregation}}
#' @return [\code{\link{Aggregation}}].
#' @examples
#' # computes the interquartile range on all performance values
#' test.iqr = makeAggregation(id = "test.iqr", name = "Test set interquartile range",
#'   fun = function (task, perf.test, perf.train, measure, group, pred) IQR(perf.test))
#' @export
makeAggregation = function(id, name = id, fun) {
  assertString(id)
  assertString(name)
  setClasses(list(id = id, name = name, fun = fun), "Aggregation")
}

#' @export
print.Aggregation = function(x, ...) {
  catf("Aggregation function: %s", x$id)
}
