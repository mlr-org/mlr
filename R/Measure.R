#' @title Construct performance measure.
#'
#' @description
#' A measure object encapsulates a function to evaluate the performance of a prediction.
#' Information about already implemented measures can be obtained here: \code{\link{measures}}.
#'
#' A learner is trained on a training set d1, results in a model m and predicts another set d2
#' (which may be a different one or the training set) resulting in the prediction.
#' The performance measure can now be defined using all of the information of the original task,
#' the fitted model and the prediction.
#'
#' Object slots:
#' \describe{
#'   \item{id [\code{character(1)}]}{See argument.}
#'   \item{minimize [\code{logical(1)}]}{See argument.}
#'   \item{properties [\code{character}]}{See argument.}
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
#'   Default is \code{TRUE}.
#' @param properties [\code{character}]\cr
#'   Set of measure properties. Some standard property names include:
#'   \describe{
#'     \item{classif}{Is the measure applicable for classification?}
#'     \item{classif.multi}{Is the measure applicable for multi-class classification?}
#'     \item{multilabel}{Is the measure applicable for multilabel classification?}
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
#' @param fun [\code{function(task, model, pred, feats, extra.args)}]\cr
#'   Calculates the performance value. Usually you will only need the prediction
#'   object \code{pred}.
#'   \describe{
#'     \item{\code{task} [\code{\link{Task}}]}{
#'       The task.}
#'     \item{\code{model} [\code{\link{WrappedModel}}]}{
#'       The fitted model.}
#'     \item{\code{pred} [\code{\link{Prediction}}]}{
#'       Prediction object.}
#'     \item{\code{feats} [\code{data.frame}]}{
#'       The features.}
#'     \item{\code{extra.args} [\code{list}]}{
#'       See below.}
#'   }
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
#'   Description and additional notes for the measure. Default is \dQuote{}.
#' @template ret_measure
#' @export
#' @family performance
#' @aliases Measure
#' @examples
#' f = function(task, model, pred, extra.args)
#'   sum((pred$data$response - pred$data$truth)^2)
#' makeMeasure(id = "my.sse", minimize = TRUE, properties = c("regr", "response"), fun = f)
makeMeasure = function(id, minimize, properties = character(0L),
  fun, extra.args = list(), aggr = test.mean, best = NULL, worst = NULL, name = id, note = "") {
  assertString(id)
  assertFlag(minimize)
  assertCharacter(properties, any.missing = FALSE)
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

  m = makeS3Obj("Measure",
    id = id,
    minimize = minimize,
    properties = properties,
    fun = fun,
    extra.args = extra.args,
    best = best,
    worst = worst,
    name = name,
    note = note
  )
  setAggregation(m, aggr)
}

#' @title Get default measure.
#'
#' @description
#' Get the default measure for a task type, task, task description or a learner.
#' Currently these are:
#'  \tabular{ll}{
#'    classif     \tab mmce\cr
#'    regr        \tab mse\cr
#'    cluster     \tab db\cr
#'    surv        \tab cindex\cr
#'    costsens    \tab mcp\cr
#'    multilabel  \tab multilabel.hamloss\cr
#'    fcregr      \tab mse
#' }
#'
#' @param x [\code{character(1)} | \code{\link{Task}} | \code{\link{TaskDesc}} | \code{\link{Learner}}]\cr
#'  Task type, task, task description, learner name, a learner, or a type of learner (e.g. "classif").
#' @return [\code{\link{Measure}}].
#' @export
getDefaultMeasure = function(x) {
  type = if (inherits(x, "TaskDesc"))
    x$type
  else if (inherits(x, "Task"))
    x$task.desc$type
  else if (inherits(x, "Learner"))
    x$type
  else if (x %in% listLearners()$class)
    stri_split_fixed(x, ".", simplify = TRUE)[1]
  else
    x
  switch(type,
    classif = mmce,
    cluster = db,
    regr = mse,
    surv = cindex,
    costsens = mcp,
    multilabel = multilabel.hamloss,
    fcregr = mse,
    mfcregr = multivar.mase,
  )
}


#' Set aggregation function of measure.
#'
#' Set how this measure will be aggregated after resampling.
#' To see possible aggregation functions: \code{\link{aggregations}}.
#'
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure.
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
