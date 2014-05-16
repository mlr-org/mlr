#' Construct performance measure.
#'
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
#' \item{id [\code{character(1)}]}{See argument.}
#' \item{minimize [\code{logical(1)}]}{See argument.}
#' \item{classif [\code{logical(1)}]}{See argument.}
#' \item{regr [\code{logical(1)}]}{See argument.}
#' \item{only.binary [\code{logical(1)}]}{See argument.}
#' \item{allowed.pred.types [\code{character}]}{See argument.}
#' \item{req.pred [\code{logical(1)}]}{Is prediction object required in calculation?}
#' \item{req.task [\code{logical(1)}]}{Is task object required in calculation?.}
#' \item{req.model [\code{logical(1)}]}{Is model object required in calculation?}
#' \item{fun [\code{function}]}{See argument.}
#' \item{extra.args [\code{list}]}{See argument.}
#' \item{aggr [\code{\link{Aggregation}}]}{See argument.}.
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Default is in \code{TRUE}.
#' @param classif [\code{logical(1)}]\cr
#'   Is the measure applicable for classification?
#'   Default is \code{FALSE}.
#' @param regr [\code{logical(1)}]\cr
#'   Is the measure applicable for regression?
#'   Default is \code{FALSE}.
#' @param costsens [\code{logical(1)}]\cr
#'   Is the measure applicable for cost-sensitive learning?
#'   Default is \code{FALSE}.
#' @param only.binary [\code{logical(1)}]\cr
#'   Is the measure only applicable to binary classification?
#'   Only reasonable if \code{classif} is \code{TRUE}.
#'   Default is \code{FALSE}.
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
#' @return [\code{\link{Measure}}].
#' @export
#' @aliases Measure
#' @examples
#' f <- function(task, model, pred, extra.args)
#'   sum((pred$data$response - pred$data$truth)^2)
#' makeMeasure(id = "my.sse", minimize = TRUE, regr = TRUE, allowed.pred.types = "response", fun = f)
makeMeasure = function(id, minimize, classif = FALSE, regr = FALSE, costsens = FALSE,
  only.binary = FALSE, allowed.pred.types = character(0L), fun, extra.args = list(), aggr = test.mean) {

  checkArg(id, "character", len = 1L, na.ok = FALSE)
  checkArg(minimize, "logical", len = 1L, na.ok = FALSE)
  checkArg(classif, "logical", len = 1L, na.ok = FALSE)
  checkArg(regr, "logical", len = 1L, na.ok = FALSE)
  checkArg(costsens, "logical", len = 1L, na.ok = FALSE)
  checkArg(only.binary, "logical", len = 1L, na.ok = FALSE)
  checkArg(allowed.pred.types, subset = c("response", "prob", "se"))
  checkArg(fun, "function")
  checkArg(extra.args, "list")

  fun1 = fun
  formals(fun1) = list()
  v = codetools::findGlobals(fun1, merge = FALSE)$variables
  if (only.binary && !classif)
    stop("only.binary can only be set to TRUE, if 'classif' is set to TRUE!")

  m = makeS3Obj("Measure",
    id = id,
    minimize = minimize,
    classif = classif,
    regr = regr,
    costsens = costsens,
    only.binary = only.binary,
    allowed.pred.types = allowed.pred.types,
    req.pred = "pred" %in% v,
    req.model = "model" %in% v,
    req.task = "task" %in% v,
    fun = fun,
    extra.args = extra.args
  )
  setAggregation(m, aggr)
}

default.measures = function(x) {
  type = if (inherits(x, "TaskDesc"))
    x$type
  else if (inherits(x, "SupervisedTask"))
    x$task.desc$type
  else if (inherits(x, "Learner"))
    x$type
  switch(type,
    classif = list(mmce),
    regr = list(mse),
    costsens = list(mcp),
    surv = list(cindex)
  )
}


#' Set aggregation function of measure.
#'
#' Set how this measure will be aggregated after resampling.
#' To see possible aggregation functions: \code{\link{aggregations}}.
#'
#' @param measure [\code{\link{Measure}}]\cr
#'   Performance measure.
#' @param aggr [\code{\link{Aggregation}}]\cr
#'   Aggregation function.
#' @return [\code{\link{Measure}}] with changed aggregation behaviour.
#' @export
setAggregation = function(measure, aggr) {
  checkArg(measure, "Measure")
  checkArg(aggr, "Aggregation")
  measure$aggr = aggr
  return(measure)
}

#' @export
print.Measure = function(x, ...) {
  catf("Performance measure: %s", x$id)
  catf("Minimize: %s", x$minimize)
  catf("Aggregated by: %s", x$aggr$id)
}

