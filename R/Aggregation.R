#' An aggregation method reduces the performance values of the test (and possibly the training sets) to a single
#' value. To see all possible, implemented aggregations look at \code{\link{aggregations}}.
#'
#' The aggregation can access all relevant information of the result after resampling and combine them into
#' a single value. Though usually something very simple like taking the mean of the test set performances is done.
#'
#' Object members:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of aggregation method.}
#' \item{fun [\code{function(task, perf.test, perf.train, measure, group, pred)}]}{Aggregation function.}
#' }
#' @name Aggregation
#' @rdname Aggregation
NULL

makeAggregation = function(id, fun) {
  checkArg(id, "character", len=1L, na.ok=FALSE)
  structure(list(id=id, fun=fun), class="Aggregation")
}

#' @S3method print Aggregation
print.Aggregation = function(x, ...) {
  catf("Aggregation function: %s", x$id)
}

