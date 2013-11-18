#' Measure performance of prediction.
#'
#' Measures the quality of a prediction w.r.t. some performance measure.
#'
#' @param pred [\code{\link{Prediction}}] \cr
#'   Prediction object to evaluate.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measure(s) to evaluate.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   Learning task, might be requested by performance measure, usually not needed.
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Model built on training data, might be requested by performance measure, usually not needed.
#' @return A single numerical performance value.
#' @export
#' @seealso \code{\link{makeMeasure}}, \code{\link{measures}}
#' @examples
#' training.set <- seq(1, nrow(iris), by = 2)
#' test.set <- seq(2, nrow(iris), by = 2)
#'
#' task <- makeClassifTask(data = iris, target = "Species")
#' lrn <- makeLearner("classif.lda")
#' mod <- train(lrn, task, subset = training.set)
#' pred <- predict(mod, newdata = iris[test.set, ])
#'
#' ## Here we define the mean misclassification error (MMCE) as our performance measure
#' my.mmce <- function(task, model, pred, extra.args) {
#'   length(which(pred$data$response != pred$data$truth)) / nrow(pred$data)
#' }
#' ms <- makeMeasure(id = "misclassification.rate",
#'                   minimize = TRUE,
#'                   classif = TRUE,
#'                   allowed.pred.types = "response",
#'                   fun = my.mmce)
#' performance(pred, ms, task, mod)
#'
#' ## Indeed the MMCE is already implemented in mlr beside other common performance measures
#' performance(pred, measures = mmce)
#'
#' ## Compute multiple performance measures at once
#' ms <- list("mmce" = mmce, "acc" = acc, "timetrain" = timetrain)
#' sapply(ms, function(the.ms) {
#'   performance(pred, measures = the.ms, task, mod)
#' })
performance = function(pred, measures, task, model) {
  if (missing(measures))
    measures = default.measures(task)[[1]]
  if (inherits(measures, "Measure"))
    measures = list(measures)
  checkListElementClass(measures, "Measure")
  td = NULL
  sapply(measures, doPerformaceIteration, pred=pred, task=task, model=model, td=td)
}

doPerformaceIteration = function(measure, pred, task, model, td){
  m = measure
  if (m$req.pred) {
    if (missing(pred))
      stopf("You need to pass pred for measure %s!", m$id)
    checkArg(pred, "Prediction")
    pred2 = pred
    td = pred$task.desc
  } else {
    pred2 = NULL
  }
  if (m$req.model) {
    if (missing(model))
      stopf("You need to pass model for measure %s!", m$id)
    checkArg(model, "WrappedModel")
    model2 = model
    td = model$task.desc
  } else {
    model2 = NULL
  }
  if (m$req.task) {
    if (missing(task))
      stopf("You need to pass task for measure %s!", m$id)
    checkArg(task, "SupervisedTask")
    task2 = task
    td = task$desc
  } else {
    task2 = NULL
  }
  # null only happens in custom resampled measure when we do no individual measurements
  if (!is.null(td)) {
    if ((td$type == "classif" && !m$classif) || (td$type == "regr" && !m$regr))
      stopf("Wrong task type %s for measure %s!", td$type, m$id)
    if (m$only.binary && length(td$class.levels) > 2)
      stopf("Multiclass problems cannot be used for measure %s!", m$id)
    if (!is.null(pred2) && !(pred2$predict.type %in% m$allowed.pred.types))
      stopf("Measure %s is only allowed for predictions of type: %s!",
            m$id, collapse(m$allowed.pred.types))
  }
  measure$fun(task2, model2, pred2, m$extra.args)
}

