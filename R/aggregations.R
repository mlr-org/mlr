#' @title Aggregation methods.
#'
#' @description
#' \itemize{
#'   \item{**test.mean**}{\cr Mean of performance values on test sets.}
#'   \item{**test.sd**}{\cr Standard deviation of performance values on test sets.}
#'   \item{**test.median**}{\cr Median of performance values on test sets.}
#'   \item{**test.min**}{\cr Minimum of performance values on test sets.}
#'   \item{**test.max**}{\cr Maximum of performance values on test sets.}
#'   \item{**test.sum**}{\cr Sum of performance values on test sets.}
#'   \item{**train.mean**}{\cr Mean of performance values on training sets.}
#'   \item{**train.sd**}{\cr Standard deviation of performance values on training sets.}
#'   \item{**train.median**}{\cr Median of performance values on training sets.}
#'   \item{**train.min**}{\cr Minimum of performance values on training sets.}
#'   \item{**train.max**}{\cr Maximum of performance values on training sets.}
#'   \item{**train.sum**}{\cr Sum of performance values on training sets.}
#'   \item{**b632**}{\cr Aggregation for B632 bootstrap.}
#'   \item{**b632plus**}{\cr Aggregation for B632+ bootstrap.}
#'   \item{**testgroup.mean**}{\cr Performance values on test sets are grouped according
#'     to resampling method. The mean for every group is calculated, then the mean of those means.
#'     Mainly used for repeated CV.}
#'   \item{**testgroup.sd**}{\cr Similar to **testgroup.mean** - after
#'     the mean for every group is calculated, the standard deviation of those means is obtained.
#'     Mainly used for repeated CV.}
#'   \item{**test.join**}{\cr Performance measure on joined test sets.
#'     This is especially useful for small sample sizes where unbalanced group sizes have a significant impact
#'     on the aggregation, especially for cross-validation test.join might make sense now.
#'     For the repeated CV, the performance is calculated on each repetition and then aggregated
#'     with the arithmetic mean.}
#' }
#' @format None
#' @seealso [Aggregation]
#' @name aggregations
#' @rdname aggregations
NULL

#' @export
#' @rdname aggregations
test.mean = makeAggregation(
  id = "test.mean",
  name = "Test mean",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.test)
)

#' @export
#' @rdname aggregations
test.sd = makeAggregation(
  id = "test.sd",
  name = "Test sd",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.test)
)

#' @export
#' @rdname aggregations
test.median = makeAggregation(
  id = "test.median",
  name = "Test median",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) median(perf.test)
)

#' @export
#' @rdname aggregations
test.min = makeAggregation(
  id = "test.min",
  name = "Test minimum",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) min(perf.test)
)

#' @export
#' @rdname aggregations
test.max = makeAggregation(
  id = "test.max",
  name = "Test maximum",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) max(perf.test)
)

#' @export
#' @rdname aggregations
test.sum = makeAggregation(
  id = "test.sum",
  name = "Test sum",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) sum(perf.test)
)

#' @export
#' @rdname aggregations
test.range = makeAggregation(
  id = "test.range",
  name = "Test range",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) diff(range(perf.test))
)

#' @export
#' @rdname aggregations
test.rmse = makeAggregation(
  id = "test.rmse",
  name = "Test RMSE",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) sqrt(mean(perf.test^2))
)

#' @export
#' @rdname aggregations
train.mean = makeAggregation(
  id = "train.mean",
  name = "Training mean",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.train)
)

#' @export
#' @rdname aggregations
train.sd = makeAggregation(
  id = "train.sd",
  name = "Training sd",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.train)
)

#' @export
#' @rdname aggregations
train.median = makeAggregation(
  id = "train.median",
  name = "Training median",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) median(perf.train)
)

#' @export
#' @rdname aggregations
train.min = makeAggregation(
  id = "train.min",
  name = "Training min",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) min(perf.train)
)

#' @export
#' @rdname aggregations
train.max = makeAggregation(
  id = "train.max",
  name = "Training max",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) max(perf.train)
)

#' @export
#' @rdname aggregations
train.sum = makeAggregation(
  id = "train.sum",
  name = "Training sum",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) sum(perf.train)
)

#' @export
#' @rdname aggregations
train.range = makeAggregation(
  id = "train.range",
  name = "Training range",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) diff(range(perf.train))
)

#' @export
#' @rdname aggregations
train.rmse = makeAggregation(
  id = "train.rmse",
  name = "Training RMSE",
  properties = "req.train",
  fun = function(task, perf.test, perf.train, measure, group, pred) sqrt(mean(perf.train^2))
)

#' @export
#' @rdname aggregations
b632 = makeAggregation(
  id = "b632",
  name = ".632 Bootstrap",
  properties = c("req.train", "req.test"),
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    mean(0.632 * perf.test + 0.368 * perf.train)
  })


# FIXME: read this again properly and double check it
#' @export
#' @rdname aggregations
b632plus = makeAggregation(
  id = "b632plus",
  name = ".632 Bootstrap plus",
  properties = c("req.train", "req.test"),
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    df = as.data.frame(pred)
    a = numeric(length(perf.test))
    for (i in seq_along(a)) {
      df2 = df[df$iter == i, , drop = FALSE]
      y1 = df2$truth
      y2 = df2$response
      grid = expand.grid(y1, y2, KEEP.OUT.ATTRS = FALSE)
      pred2 = makePrediction(
        task.desc = pred$task.desc, row.names = rownames(grid),
        id = NULL, truth = grid[, 1L], predict.type = "response", y = grid[, 2L],
        time = NA_real_
      )
      gamma = performance(pred2, measures = measure)
      R = (perf.test[i] - perf.train[i]) / (gamma - perf.train[i])
      w = 0.632 / (1 - 0.368 * R)
      a[i] = (1 - w) * perf.train[i] + w * perf.test[i]
    }
    return(mean(a))
  })

#' @export
#' @rdname aggregations
testgroup.mean = makeAggregation(
  id = "testgroup.mean",
  name = "Test group mean",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    mean(vnapply(split(perf.test, group), mean))
  })

#' @export
#' @rdname aggregations
testgroup.sd = makeAggregation(
  id = "testgroup.sd",
  name = "Test group standard deviation",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    sd(BBmisc::vnapply(split(perf.test, group), mean))
  })

#' @export
#' @rdname aggregations
test.join = makeAggregation(
  id = "test.join",
  name = "Test join",
  properties = "req.test",
  fun = function(task, perf.test, perf.train, measure, group, pred) {
    df = as.data.frame(pred)
    f = if (length(group)) group[df$iter] else factor(rep(1L, nrow(df)))
    mean(vnapply(split(df, f), function(df) {
      if (pred$predict.type == "response") y = df$response
      if (pred$predict.type == "prob") {
        y = df[, stri_startswith_fixed(colnames(df), "prob."), drop = FALSE]
        colnames(y) = stri_sub(colnames(y), 6L)
      }
      npred = makePrediction(
        task.desc = pred$task.desc, row.names = rownames(df),
        id = NULL, truth = df$truth, predict.type = pred$predict.type, y = y,
        time = NA_real_
      )
      performance(npred, measure)
    }))
  })
