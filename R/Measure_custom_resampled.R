#' @title Construct your own resampled performance measure.
#'
#' @description
#' Construct your own performance measure, used after resampling.
#' Note that individual training / test set performance values will be set to `NA`, you
#' only calculate an aggregated value. If you can define a function that makes sense
#' for every single training / test set, implement your own [Measure].
#'
#' @param measure.id (`character(1)`)\cr
#'   Short name of measure.
#' @param measure.name (`character(1)`)\cr
#'   Long name of measure.
#'   Default is `measure.id`.
#' @param aggregation.id (`character(1)`)\cr
#'   Short name of aggregation.
#' @param aggregation.name (`character(1)`)\cr
#'   Long name of the aggregation.
#'   Default is `aggregation.id`.
#' @param fun (`function(task, group, pred, extra.args)`)\cr
#'   Calculates performance value from [ResamplePrediction] object.
#'   For rare cases you can also use the task, the grouping or the extra arguments `extra.args`.
#'   \describe{
#'     \item{`task` ([Task])}{
#'       The task.}
#'     \item{`group` ([factor])}{
#'       Grouping of resampling iterations. This encodes whether specific iterations
#'       'belong together' (e.g. repeated CV).}
#'     \item{`pred` ([Prediction])}{
#'       Prediction object.}
#'     \item{`extra.args` ([list])}{
#'       See below.}
#'   }
#' @param extra.args ([list])\cr
#'   List of extra arguments which will always be passed to `fun`.
#'   Default is empty list.
#' @param minimize (`logical(1)`)\cr
#'   Should the measure be minimized?
#'   Default is `TRUE`.
#' @param properties ([character])\cr
#'   Set of measure properties. For a list of values see [Measure].
#'   Default is `character(0)`.
#' @param best (`numeric(1)`)\cr
#'   Best obtainable value for measure.
#'   Default is -`Inf` or `Inf`, depending on `minimize`.
#' @param worst (`numeric(1)`)\cr
#'   Worst obtainable value for measure.
#'   Default is `Inf` or -`Inf`, depending on `minimize`.
#' @param note ([character]) \cr
#'   Description and additional notes for the measure. Default is \dQuote{}.
#' @template ret_measure
#' @family performance
#' @noMd
#' @export
makeCustomResampledMeasure = function(measure.id, aggregation.id, minimize = TRUE, properties = character(0L),
  fun, extra.args = list(), best = NULL, worst = NULL, measure.name = measure.id,
  aggregation.name = aggregation.id, note = "") {

  assertString(measure.id)
  assertString(aggregation.id)
  assertFlag(minimize)
  assertCharacter(properties, any.missing = FALSE)
  assertFunction(fun)
  assertList(extra.args)
  assertString(measure.name)
  assertString(aggregation.name)
  assertString(note)

  force(fun)
  fun1 = function(task, model, pred, feats, extra.args) NA_real_
  # args are checked here
  custom = makeMeasure(id = measure.id, minimize, properties, fun1, extra.args,
    best = best, worst = worst, name = measure.name, note = note)
  fun2 = function(task, perf.test, perf.train, measure, group, pred) {
    fun(task, group, pred, extra.args)
  }
  # we set the properties to "no requirements" here
  aggr = makeAggregation(id = aggregation.id, name = aggregation.name, properties = character(0L), fun = fun2)
  setAggregation(custom, aggr)
}
