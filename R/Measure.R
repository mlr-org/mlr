#' @title Construct performance measure.
#'
#' @description
#' A measure object encapsulates a function to evaluate the performance of a
#' prediction. Information about already implemented measures can be obtained
#' here: [measures].
#'
#' A learner is trained on a training set d1, results in a model m and predicts
#' another set d2 (which may be a different one or the training set) resulting
#' in the prediction. The performance measure can now be defined using all of
#' the information of the original task, the fitted model and the prediction.
#'
#' @param id (`character(1)`)\cr
#'   Name of measure.
#' @param minimize (`logical(1)`)\cr
#'   Should the measure be minimized?
#'   Default is `TRUE`.
#' @param properties ([character])\cr
#'   Set of measure properties. Some standard property names include:
#'     - classif: Is the measure applicable for classification?
#'     - classif.multi: Is the measure applicable for multi-class classification?
#'     - multilabel: Is the measure applicable for multilabel classification?
#'     - regr: Is the measure applicable for regression?
#'     - surv: Is the measure applicable for survival?
#'     - cluster: Is the measure applicable for cluster?
#'     - costsens: Is the measure applicable for cost-sensitive learning?
#'     - req.pred: Is prediction object required in calculation? Usually the case.
#'     - req.truth: Is truth column required in calculation? Usually the case.
#'     - req.task: Is task object required in calculation? Usually not the case
#'     - req.model: Is model object required in calculation? Usually not the case.
#'     - req.feats: Are feature values required in calculation? Usually not the case.
#'     - req.prob: Are predicted probabilities required in calculation? Usually not the case, example would be AUC.
#'
#'   Default is `character(0)`.
#' @param fun (`function(task, model, pred, feats, extra.args)`)\cr
#'   Calculates the performance value. Usually you will only need the prediction
#'   object `pred`.
#'     - `task` ([Task])\cr
#'       The task.
#'     - `model` ([WrappedModel])\cr
#'       The fitted model.
#'     - `pred` ([Prediction])\cr
#'       Prediction object.
#'     - `feats` ([data.frame])\cr
#'       The features.
#'     - `extra.args` ([list])\cr
#'       See below.
#' @param extra.args ([list])\cr
#'   List of extra arguments which will always be passed to `fun`.
#'   Can be changed after construction via [setMeasurePars()].
#'   Default is empty list.
#' @param aggr ([Aggregation])\cr
#'   Aggregation function, which is used to aggregate the values measured
#'   on test / training sets of the measure to a single value.
#'   Default is [test.mean].
#' @param best (`numeric(1)`)\cr
#'   Best obtainable value for measure.
#'   Default is -`Inf` or `Inf`, depending on `minimize`.
#' @param worst (`numeric(1)`)\cr
#'   Worst obtainable value for measure.
#'   Default is `Inf` or -`Inf`, depending on `minimize`.
#' @param name ([character]) \cr
#'   Name of the measure. Default is `id`.
#' @param note ([character]) \cr
#'   Description and additional notes for the measure. Default is \dQuote{}.
#' @template ret_measure
#' @export
#' @family performance
#' @aliases Measure
#' @examples
#' f = function(task, model, pred, extra.args) {
#'   sum((pred$data$response - pred$data$truth)^2)
#' }
#' makeMeasure(id = "my.sse", minimize = TRUE,
#'   properties = c("regr", "response"), fun = f)
makeMeasure = function(id, minimize, properties = character(0L),
  fun, extra.args = list(), aggr = test.mean, best = NULL, worst = NULL, name = id, note = "") {

  assertString(id)
  assertFlag(minimize)
  assertCharacter(properties, any.missing = FALSE)
  assertFunction(fun)
  assertList(extra.args)
  assertString(note)
  if (is.null(best)) {
    best = ifelse(minimize, -Inf, Inf)
  } else {
    assertNumber(best)
  }
  if (is.null(worst)) {
    worst = ifelse(minimize, Inf, -Inf)
  } else {
    assertNumber(worst)
  }

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
#'  classif: mmce\cr
#'  regr: mse\cr
#'  cluster: db\cr
#'  surv: cindex\cr
#'  costsen: mcp\cr
#'  multilabel: multilabel.hamloss\cr
#'
#' @param x ([character(1)` | [Task] | [TaskDesc] | [Learner])\cr
#'  Task type, task, task description, learner name, a learner, or a type of learner (e.g. "classif").
#' @return ([Measure]).
#' @export
getDefaultMeasure = function(x) {
  type = if (inherits(x, "TaskDesc")) {
    x$type
  } else if (inherits(x, "Task")) {
    x$task.desc$type
  } else if (inherits(x, "Learner")) {
    x$type
  } else if (x %in% listLearners()$class) {
    stri_split_fixed(x, ".", simplify = TRUE)[1]
  } else {
    x
  }
  switch(type,
    classif = mmce,
    cluster = db,
    regr = mse,
    surv = cindex,
    costsens = mcp,
    multilabel = multilabel.hamloss
  )
}

#' @export
print.Measure = function(x, ...) {
  catf("Name: %s", x$name)
  catf("Performance measure: %s", x$id)
  catf("Properties: %s", collapse(x$properties))
  catf("Minimize: %s", x$minimize)
  catf("Best: %g; Worst: %g", x$best, x$worst)
  catf("Aggregated by: %s", x$aggr$id)
  catf("Arguments: %s", listToShortString(x$extra.args))
  catf("Note: %s", x$note)
}
