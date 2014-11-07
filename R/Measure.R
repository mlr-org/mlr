#' @title Construct performance measure.
#'
#' @description
#' A measure object encapsulates a function to evaluate the performance of a prediction.
#' Information about already implemented measures can be obtained here: \code{\link{measures}}.
#'
#' A learner is trained on a a training set d1, results in a model m, predicts another set d2
#' (which may be a different one or the training set), resulting in the prediction.
#' The performance measure can now be defined using all of the information of the original task,
#' the fitted model and the prediction.
#'
#' Object slots:
#' \describe{
#'   \item{id [\code{character(1)}]}{See argument.}
#'   \item{minimize [\code{logical(1)}]}{See argument.}
#'   \item{properties [\code{character}]}{See argument.}
#'   \item{allowed.pred.types [\code{character}]}{See argument.}
#'   \item{req.pred [\code{logical(1)}]}{Is prediction object required in calculation?}
#'   \item{req.task [\code{logical(1)}]}{Is task object required in calculation?}
#'   \item{req.model [\code{logical(1)}]}{Is model object required in calculation?}
#'   \item{req.feats [\code{logical(1)}]}{Is feature object required in calculation?}
#'   \item{fun [\code{function}]}{See argument.}
#'   \item{extra.args [\code{list}]}{See argument.}
#'   \item{aggr [\code{\link{Aggregation}}]}{See argument.}
#'   \item{best [\code{numeric(1)}]}{See argument.}
#'   \item{worst [\code{numeric(1)}]}{See argument.}
#'   \item{name [\code{character(1)}]}{See argument.}
#'   \item{note [\code{character(1)}]}{See argument.}
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Default is in \code{TRUE}.
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
#' @param allowed.pred.types [\code{character}]\cr
#'   Which prediction types are allowed for this measure?
#'   Subset of \dQuote{response},\dQuote{prob}, \dQuote{se}.
#'   Default is \code{character(0)}.
#' @param fun [\code{function(task, model, pred, extra.args)}]\cr
#'   Calculates performance value.
#' @param extra.args [\code{list}]\cr
#'   List of extra arguments which will always be passed to \code{fun}.
#'   Default is empty list.
#' @param aggr [\code{\link{Aggregation}}]\cr
#'   Aggregation funtion, which is used to aggregate the values measured
#'   on test / training sets of the measure to a single value.
#'   Default is \code{\link{test.mean}}.
#' @param best [\code{numeric(1)}]\cr
#'   Best obtainable value for measure.
#'   Default is -\code{Inf} or \code{Inf}, depending on \code{minimize}.
#' @param worst [\code{numeric(1)}]\cr
#'   Worst obtainable value for measure.
#'   Default is \code{Inf} or -\code{Inf}, depending on \code{minimize}.
#' @param name [\code{character}] \cr
#'   Name of the measure. Default is \code{id}.
#' @param note [\code{character}] \cr
#'   Description and additional notes for the learner. Default is \dQuote{}.
#' @template ret_measure
#' @export
#' @family performance
#' @aliases Measure
#' @examples
#' f = function(task, model, pred, extra.args)
#'   sum((pred$data$response - pred$data$truth)^2)
#' makeMeasure(id = "my.sse", minimize = TRUE, properties = c("regr", "response"), fun = f)
makeMeasure = function(id, minimize, properties = character(0L), allowed.pred.types = character(0L),
  fun, extra.args = list(), aggr = test.mean, best = NULL, worst = NULL, name = id, note = "") {
  assertString(id)
  assertFlag(minimize)
  assertCharacter(properties, any.missing = FALSE)
  assertSubset(allowed.pred.types, choices = c("response", "prob", "se"))
  assertFunction(fun)
  assertList(extra.args)
  assertString(note)
  if (is.null(best))
    best = ifelse(minimize, -Inf, Inf)
  else
    assertNumber(best)
  if (is.null(worst))
    worst = ifelse(minimize, Inf, -Inf)
  else
    assertNumber(worst)

  # FIXME: I think this is never used...
  fun1 = fun
  formals(fun1) = list()
  v = codetools::findGlobals(fun1, merge = FALSE)$variables

  m = makeS3Obj("Measure",
    id = id,
    minimize = minimize,
    properties = properties,
    allowed.pred.types = allowed.pred.types,
    req.pred = "pred" %in% v,
    req.model = "model" %in% v,
    req.task = "task" %in% v,
    req.feats = "feats" %in% v,
    fun = fun,
    extra.args = extra.args,
    best = best,
    worst = worst,
    name = name,
    note = note
  )
  setAggregation(m, aggr)
}

default.measures = function(x) {
  type = if (inherits(x, "TaskDesc"))
    x$type
  else if (inherits(x, "Task"))
    x$task.desc$type
  else if (inherits(x, "Learner"))
    x$type
  switch(type,
    classif = list(mmce),
    regr = list(mse),
    costsens = list(mcp),
    surv = list(cindex),
    cluster = list(db)
  )
}


#' Set aggregation function of measure.
#'
#' Set how this measure will be aggregated after resampling.
#' To see possible aggregation functions: \code{\link{aggregations}}.
#'
#' @template arg_measure
#' @template arg_aggr
#' @return [\code{\link{Measure}}] with changed aggregation behaviour.
#' @export
setAggregation = function(measure, aggr) {
  assertClass(measure, classes = "Measure")
  assertClass(aggr, classes = "Aggregation")
  measure$aggr = aggr
  return(measure)
}

#' @export
print.Measure = function(x, ...) {
  catf("Name: %s", x$name)
  catf("Performance measure: %s", x$id)
  catf("Properties: %s", collapse(x$properties))
  catf("Minimize: %s", x$minimize)
  catf("Best: %g; Worst: %g", x$best, x$worst)
  catf("Aggregated by: %s", x$aggr$id)
  catf("Note: %s", x$note)
}
