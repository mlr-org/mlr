#' @title Construct your own resampled performance measure.
#'
#' @description
#' Construct your own performance measure, used after resampling.
#' Note that individual training / test set performance values will be set to \code{NA}, you
#' only calculate an aggregated value. If you can define a function that makes sense
#' for every single training / test set, implement your own \code{\link{Measure}}.
#'
#' @param measure.id [\code{character(1)}]\cr
#'   Short name of measure.
#' @param measure.name [\code{character(1)}]\cr
#'   Long name of measure.
#'   Default is \code{measure.id}.
#' @param aggregation.id [\code{character(1)}]\cr
#'   Short name of aggregation.
#' @param aggregation.name [\code{character(1)}]\cr
#'   Long name of the aggregation.
#'   Default is \code{aggregation.id}.
#' @param fun [\code{function(task, group, pred, extra.args)}]\cr
#'   Calculates performance value from \code{\link{ResamplePrediction}} object.
#'   For rare cases you can also use the task, the grouping or the extra arguments \code{extra.args}.
#'   \describe{
#'     \item{\code{task} [\code{\link{Task}}]}{
#'       The task.}
#'     \item{\code{group} [\code{factor}]}{
#'       Grouping of resampling iterations. This encodes whether specific iterations
#'       'belong together' (e.g. repeated CV).}
#'     \item{\code{pred} [\code{\link{Prediction}}]}{
#'       Prediction object.}
#'     \item{\code{extra.args} [\code{list}]}{
#'       See below.}
#'   }
#' @param extra.args [\code{list}]\cr
#'   List of extra arguments which will always be passed to \code{fun}.
#'   Default is empty list.
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
#'     \item{costsens}{Is the measure applicable for cost-sensitive learning?}
#'     \item{req.pred}{Is prediction object required in calculation? Usually the case.}
#'     \item{req.truth}{Is truth column required in calculation? Usually the case.}
#'     \item{req.task}{Is task object required in calculation? Usually not the case}
#'     \item{req.model}{Is model object required in calculation? Usually not the case.}
#'     \item{req.feats}{Are feature values required in calculation? Usually not the case.}
#'     \item{req.prob}{Are predicted probabilites required in calculation? Usually not the case, example would be AUC.}
#'   }
#'   Default is \code{character(0)}.
#' @param best [\code{numeric(1)}]\cr
#'   Best obtainable value for measure.
#'   Default is -\code{Inf} or \code{Inf}, depending on \code{minimize}.
#' @param worst [\code{numeric(1)}]\cr
#'   Worst obtainable value for measure.
#'   Default is \code{Inf} or -\code{Inf}, depending on \code{minimize}.
#' @param note [\code{character}] \cr
#'   Description and additional notes for the measure. Default is \dQuote{}.
#' @template ret_measure
#' @family performance
#' @export
makeCustomResampledMeasure = function(measure.id, aggregation.id, minimize = TRUE, properties = character(0L),
                                      fun, extra.args = list(), best = NULL, worst = NULL,
                                      measure.name = measure.id, aggregation.name = aggregation.id,
                                      note = "") {
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
  fun2 = function(task, perf.test, perf.train, measure, group, pred)
    fun(task, group, pred, extra.args)
  aggr = makeAggregation(id = aggregation.id, name = aggregation.name, fun = fun2)
  setAggregation(custom, aggr)
}
