#' @title Aggregation object.
#' @description
#' An aggregation method reduces the performance values of the test
#' (and possibly the training sets) to a single value.
#' To see all possible, implemented aggregations look at \code{\link{aggregations}}.
#'
#' The aggregation can access all relevant information of the result after resampling
#' and combine them into a single value. Though usually something very simple
#' like taking the mean of the test set performances is done.
#'
#' Object members:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of aggregation method.}
#' \item{fun [\code{function(task, perf.test, perf.train, measure, group, pred)}]}{Aggregation function.}
#' }
#' @name Aggregation
#' @seealso \code{\link{makeAggregation}}
#' @rdname Aggregation
NULL


#' @title Specifiy your own aggregation of measures
#'
#' @description
#' This is an adavanced feature of mlr. It gives access to some
#' inner workings so the result might not be compatible with everything! \cr
#'
#'
#' @param id [\code{character(1)}]\cr
#'   Name of the aggregation method. (Preferably the same name as the generated function)
#' @param fun [\code{function}]\cr
#'   A function with following signature: \code{function(task, perf.test, perf.train, measure, group, pred)}
#'   \itemize{
#'    \item{\bold{task}}: task (\code{\link{Task}}) object
#'    \item{\bold{perf.test}}: numerical vector of \link{performance} results on the test data set
#'    \item{\bold{perf.train}}. numerical vector of \link{performance} results on the train data set
#'    \item{\bold{measure}}: \code{\link{Measure}} object.
#'    \item{\bold{group}}: grouping vector
#'    \item{\bold{pred}}: \code{\link{Prediction}} object
#'   }
#' @seealso \link{aggregations}, \code{\link{setAggregation}}
#' @return \link{Aggregation} object
#' @examples
#' # computes the interquartile range on all performance values
#' test.iqr = makeAggregation(id = "test.iqr",
#'   fun = function (task, perf.test, perf.train, measure, group, pred) IQR(perf.test))
#' @export
makeAggregation = function(id, fun) {
  assertString(id)
  setClasses(list(id = id, fun = fun), "Aggregation")
}

#' @export
print.Aggregation = function(x, ...) {
  catf("Aggregation function: %s", x$id)
}
