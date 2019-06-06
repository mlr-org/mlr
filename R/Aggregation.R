#' @title Aggregation object.
#' @description
#' An aggregation method reduces the performance values of the test
#' (and possibly the training sets) to a single value.
#' To see all possible implemented aggregations look at [aggregations].
#'
#' The aggregation can access all relevant information of the result after resampling
#' and combine them into a single value. Though usually something very simple
#' like taking the mean of the test set performances is done.
#'
#' Object members:
#' \describe{
#' \item{id (`character(1)`)}{Name of the aggregation method.}
#' \item{name (`character(1)`)}{Long name of the aggregation method.}
#' \item{properties ([character])}{Properties of the aggregation.}
#' \item{fun (`function(task, perf.test, perf.train, measure, group, pred)])}{Aggregation function.}
#' }
#' @name Aggregation
#' @seealso [makeAggregation]
#' @rdname Aggregation
NULL


#' @title Specify your own aggregation of measures.
#'
#' @description
#' This is an advanced feature of mlr. It gives access to some
#' inner workings so the result might not be compatible with everything!
#'
#' @param id (`character(1)`)\cr
#'   Name of the aggregation method (preferably the same name as the generated function).
#' @param name (`character(1)`)\cr
#'   Long name of the aggregation method. Default is `id`.
#' @param properties ([character])\cr
#'   Set of aggregation properties.
#'   \describe{
#'     \item{req.train}{Are prediction or train sets required to calculate the aggregation?}
#'     \item{req.test}{Are prediction or test sets required to calculate the aggregation?}
#'   }
#' @param fun (`function(task, perf.test, perf.train, measure, group, pred)`)\cr
#'   Calculates the aggregated performance. In most cases you will only need the performances
#'   `perf.test` and optionally `perf.train` on the test and training data sets.
#'   \describe{
#'     \item{`task` ([Task])}{The task.}
#'     \item{`perf.test` ([numeric])}{
#'       [performance] results on the test data sets.}
#'     \item{`perf.train` ([numeric])}{
#'       [performance] results on the training data sets.}
#'     \item{`measure` ([Measure])}{
#'       Performance measure.}
#'     \item{`group` ([factor])}{
#'       Grouping of resampling iterations. This encodes whether specific iterations
#'       'belong together' (e.g. repeated CV).}
#'     \item{`pred` ([Prediction])}{
#'       Prediction object.}
#'   }
#' @seealso [aggregations], [setAggregation]
#' @return ([Aggregation]).
#' @examples
#' # computes the interquartile range on all performance values
#' test.iqr = makeAggregation(
#'   id = "test.iqr", name = "Test set interquartile range",
#'   properties = "req.test",
#'   fun = function(task, perf.test, perf.train, measure, group, pred) IQR(perf.test)
#' )
#' @export
makeAggregation = function(id, name = id, properties, fun) {
  assertString(id)
  assertString(name)
  makeS3Obj("Aggregation", id = id, name = name, fun = fun, properties = properties)
}

#' @export
print.Aggregation = function(x, ...) {
  catf("Aggregation function: %s", x$id)
}
