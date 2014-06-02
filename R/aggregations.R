#' Aggregation methods.
#'
#' \itemize{
#' \item{\bold{test.mean}}{\cr Mean of performance values on test sets.}
#' \item{\bold{test.sd}}{\cr Standard deviation of performance values on test sets.}
#' \item{\bold{test.median}}{\cr Median of performance values on test sets.}
#' \item{\bold{test.min}}{\cr Minimum of performance values on test sets.}
#' \item{\bold{test.max}}{\cr Maximum of performance values on test sets.}
#' \item{\bold{test.sum}}{\cr Sum of performance values on test sets.}
#' \item{\bold{train.mean}}{\cr Mean of performance values on training sets.}
#' \item{\bold{train.sd}}{\cr Standard deviation of performance values on training sets.}
#' \item{\bold{train.median}}{\cr Median of performance values on training sets.}
#' \item{\bold{train.min}}{\cr Minimum of performance values on training sets.}
#' \item{\bold{train.max}}{\cr Maximum of performance values on training sets.}
#' \item{\bold{train.sum}}{\cr Sum of performance values on training sets.}
#' \item{\bold{b632}}{\cr Aggregation for B632 bootstrap.}
#' \item{\bold{b632plus}}{\cr Aggregation for B632+ bootstrap.}
#' \item{\bold{testgroup.mean}}{\cr Performance values on test sets are grouped according
#'   to resampling method. The mean for very group is calculated, then the mean of those means.
#'   Mainly used for repeated CV.}
#' }
#' @format None
#' @seealso \code{\link{Aggregation}}
#' @name aggregations
#' @rdname aggregations
NULL

#' @export
#' @rdname aggregations
test.mean = makeAggregation(
  id = "test.mean",
  fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.test)
)

#' @export
#' @rdname aggregations
test.sd = makeAggregation(
  id = "test.sd",
  fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.test)
)

#' @export
#' @rdname aggregations
test.median = makeAggregation(
  id = "test.median",
  fun = function(task, perf.test, perf.train, measure, group, pred) median(perf.test)
)

#' @export
#' @rdname aggregations
test.min = makeAggregation(
  id = "test.min",
  fun = function(task, perf.test, perf.train, measure, group, pred) min(perf.test)
)

#' @export
#' @rdname aggregations
test.max = makeAggregation(
  id = "test.max",
  fun = function(task, perf.test, perf.train, measure, group, pred) max(perf.test)
)

#' @export
#' @rdname aggregations
test.sum = makeAggregation(
  id = "test.sum",
  fun = function(task, perf.test, perf.train, measure, group, pred) sum(perf.test)
)

#' @export
#' @rdname aggregations
test.range = makeAggregation(
  id = "test.range",
  fun = function(task, perf.test, perf.train, measure, group, pred) diff(range(perf.test))
)

#' @export
#' @rdname aggregations
test.sqrt.of.mean = makeAggregation(
  id = "test.sqrt.of.mean",
  fun = function(task, perf.test, perf.train, measure, group, pred) sqrt(mean(perf.test))
)

#' @export
#' @rdname aggregations
train.mean = makeAggregation(
  id = "train.mean",
  fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.train)
)

#' @export
#' @rdname aggregations
train.sd = makeAggregation(
  id = "train.sd",
  fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.train)
)

#' @export
#' @rdname aggregations
train.median = makeAggregation(
  id = "train.median",
  fun = function(task, perf.test, perf.train, measure, group, pred) median(perf.train)
)

#' @export
#' @rdname aggregations
train.min = makeAggregation(
  id = "train.min",
  fun = function(task, perf.test, perf.train, measure, group, pred) min(perf.train)
)

#' @export
#' @rdname aggregations
train.max = makeAggregation(
  id = "train.max",
  fun = function(task, perf.test, perf.train, measure, group, pred) max(perf.train)
)

#' @export
#' @rdname aggregations
train.sum = makeAggregation(
  id = "train.sum",
  fun = function(task, perf.test, perf.train, measure, group, pred) sum(perf.train)
)

#' @export
#' @rdname aggregations
train.range = makeAggregation(
  id = "train.range",
  fun = function(task, perf.test, perf.train, measure, group, pred) diff(range(perf.train))
)

#' @export
#' @rdname aggregations
train.sqrt.of.mean = makeAggregation(
  id = "train.sqrt.of.mean",
  fun = function(task, perf.test, perf.train, measure, group, pred) sqrt(mean(perf.train))
)

#' @export
#' @rdname aggregations
b632 = makeAggregation(
  id = "b632",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    mean(0.632*perf.test + 0.368*perf.train)
  }
)


#FIXME read this again properly and double check it
#' @export
#' @rdname aggregations
b632plus = makeAggregation(
  id = "b632plus",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    df = as.data.frame(pred)
    a = numeric(length(perf.test))
    for (i in seq_along(a)) {
      df2 = df[df$iter == i, ]
      y1 = df2$truth
      y2 = df2$response
      grid = expand.grid(y1, y2, KEEP.OUT.ATTRS=FALSE)
      pred2 = makePrediction(task.desc=pred$task.desc,
        id=NULL, truth=grid[, 1L], predict.type="response", y=grid[, 2L],
        time=NA_real_)
      gamma = performance(pred2, measures=measure)
      R = (perf.test[i] - perf.train[i]) / (gamma - perf.train[i])
      w = 0.632 / (1 - 0.368*R)
      a[i] = (1-w) * perf.train[i] + w*perf.test[i]
    }
    return(mean(a))
  }
)

#' @export
#' @rdname aggregations
testgroup.mean = makeAggregation(
  id = "testgroup.mean",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    mean(sapply(split(perf.test, group), mean))
  }
)
