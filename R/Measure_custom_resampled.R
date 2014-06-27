#' Construct your own resampled performance measure.
#'
#' Construct your own performance measure, used after resampling.
#' Note that individual training / test set performance values will be set to \code{NA}, you
#' only calculate an aggregated value. If you can define a function that makes sense
#' for every single training / test set, implement your own \code{\link{Measure}}.
#'
#' @param id [\code{character(1)}]\cr
#'   Name of aggregated measure.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Default is \code{TRUE}.
#' @param properties [\code{character}]\cr
#'   Set of measure properties. Some standard property names include:
#'   \describe{
#'     \item{classif}{Is the measure applicable for classification?}
#'     \item{classif.multi}{Is the measure applicable for multi-class classification?}
#'     \item{regr}{Is the measure applicable for regression?}
#'     \item{surv}{Is the measure applicable for survival?}
#'     \item{costsens}{Is the measure applicable for cost-sensitve learning?}
#'   }
#'   Default is \code{character(0)}.
#' @param allowed.pred.types [\code{character}]
#'   Which prediction types are allowed for this measure?
#'   Subset of \dQuote{response},\dQuote{prob},\dQuote{se}.
#'   Default is none.
#' @param fun [\code{function(task, pred, group, pred, extra.args)}]\cr
#'   Calculates performance value from \code{\link{ResamplePrediction}} object.
#'   For rare case you can also use the task, the grouping or the extra arguments \code{extra.args}.
#' @param extra.args [\code{list}]\cr
#'   List of extra arguments which will always be passed to \code{fun}.
#'   Default is empty list.
#' @return \code{\link{Measure}}
#' @export
makeCustomResampledMeasure = function(id, minimize = TRUE, properties = character(0L),
  allowed.pred.types = character(0L), fun, extra.args = list()) {

  assertString(id)
  assertFlag(minimize)
  assertCharacter(properties, any.missing = FALSE)
  assertSubset(allowed.pred.types, choices = c("response", "prob", "se"))
  assertFunction(fun)
  assertList(extra.args)

  force(fun)
  fun1 = function(task, model, pred, extra.args) NA_real_
  # args are checked here
  custom = makeMeasure(id = "custom", minimize, properties, allowed.pred.types, fun1, extra.args)
  fun2 = function(task, perf.test, perf.train, measure, group, pred)
    fun(task, group, pred, extra.args)
  aggr = makeAggregation(id = id, fun = fun2)
  setAggregation(custom, aggr)
}
